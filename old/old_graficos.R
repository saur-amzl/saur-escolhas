
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(ggplot2, dplyr, tidyr,stringr)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


# GRAFICO 1: GASTOS ---------------------------------------------------
tabela_gastos <- read.csv(paste(pathdir,"tabela_gastos_uc_2008_2018_amzlegal_8dec2024.csv", sep = ""), sep=";")
tabela_gastos <- tabela_gastos %>% mutate(descricao = recode(descricao, 
                          "Alimentação no domicílio" = "No Domicílio", 
                          "Alimentação fora do domicílio" = "Fora do Domicílio"))

tabela_gastos$descricao_estrato

# Selecionar os dois estratos
descricao_estrato_selecionados <- c('AC-UCapital', 'AC-UF','AM-UCapital', 'AM-UF',
                                    'AP-UCapital', 'AP-UF','MA-UCapital', 'MA-UF',
                                    'MT-UCapital', 'MT-UF','PA-CapitalRRM', 'PA-UF',
                                    'RO-UCapital', 'RO-UF','RR-UCapital', 'RR-UF',
                                    'TO-UCapital', 'TO-UF')
dados_filtrados <- tabela_gastos %>%
  filter(descricao_estrato %in% descricao_estrato_selecionados) %>%
  mutate(estado = str_sub(descricao_estrato, 1, 2),
         nivel = str_sub(descricao_estrato, 4, 14)) %>%
  mutate(nome_estado = recode(estado,
                                 AC = "Acre",
                                 AM = "Amazonas",
                                 AP = "Amapá",
                                 MA = "Maranhão",
                                 MT = "Mato Grosso",
                                 PA = "Pará",
                                 RO = "Rondônia",
                                 RR = "Roraima",
                                 TO = "Tocantins"),
         legenda_nivel = recode(nivel,
                                UCapital= 'Capital',
                                UF= 'Estado',
                                CapitalRRM = 'Região Metropolitana'))
head(dados_filtrados)


# Reestruturar os dados para formato longo
dados_long <- dados_filtrados %>%
  pivot_longer(cols = starts_with("gasto_mensal_familiar"), 
               names_to = "ano", 
               values_to = "gasto") %>%
  mutate(ano = gsub("gasto_mensal_familiar_", "", ano))  # Ajustar o nome do ano


# Criar o gráfico em linha
p1 <-ggplot(dados_long, aes(x = ano, y = gasto, group = interaction(descricao_estrato, descricao))) +
  geom_line(aes(color = legenda_nivel, linetype = descricao), size = 1.2) +  # Linha com espessura maior
  scale_linetype_manual(values = c("No Domicílio" = "solid", 
                                   "Fora do Domicílio" = "dotdash")) +  # Tipo de linha (solid ou dashed)
  scale_color_manual(values = c("Estado" = "#1f78b4",  # Azul para Estado
                                "Capital" = "#33a02c",  # Verde para Capital
                                "Região Metropolitana" = "#e31a1c")) +  # Vermelho para Região Metropolitana
  labs(x = "",y = "Gasto Mensal (R$)",
       color = "",  # Cor para Estado/Capital
       linetype = "Categoria",   # Linha para Dentro ou Fora do Município
       caption = "Fonte: Pesquisa de Orçamentos Familiares (POF)") +  # Fonte
  theme_bw(base_size = 14) +  # Tema minimalista com base tamanho de fonte maior
  theme(
    #legend.position = "top",  # Coloca a legenda embaixo
    legend.title = element_text(size = 12, face = "bold"),  # Título da legenda em negrito
    legend.text = element_text(size = 11),  # Texto da legenda com tamanho ajustado
    axis.title = element_text(size = 14, face = "bold"),  # Títulos dos eixos em negrito
    axis.text = element_text(size = 12),  # Texto dos eixos
    strip.text = element_text(size = 14, face = "bold"),  # Título do facet (estado)
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Título do gráfico centralizado e em negrito
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),  # Subtítulo em itálico
    plot.caption = element_text(size = 10, hjust = 1)  # Fonte do gráfico no canto inferior direito
    #axis.text.x = element_text(margin = margin(t = 1)),  # Reduzir o espaço acima dos rótulos no eixo x
    #axis.text.y = element_text(margin = margin(r = 1)),
    #plot.margin = margin(1, 1, 10, 10)  # Ajuste das margens externas do gráfico
  ) +
  facet_wrap(~ nome_estado, scales = "free_y") +  # Facet por estado
  #geom_text(aes(label = round(gasto, 0)), 
  #          position = position_nudge(y = 10),  # Ajustar o deslocamento dos rótulos
  #          size = 4,  # Tamanho da fonte
  #          color = "black",
  #          vjust = -0.1) +  # Cor do texto
  scale_y_continuous(limits = c(0, 210))  # Define o intervalo do eixo X de 0 a 200
p1
ggsave("grafico_gasto_mensal.png", plot = p1, 
       width = 10, height = 6, dpi = 300)  # Definindo o tamanho e a resolução



# Filtrar apenas Capitais e Regiões Metropolitanas
dados_barras <- dados_filtrados %>%
  filter(legenda_nivel %in% c("Capital", "Região Metropolitana"))

# Reestruturar os dados para formato longo
dados_long_barras <- dados_barras %>%
  pivot_longer(cols = starts_with("gasto_mensal_familiar"), 
               names_to = "ano", 
               values_to = "gasto") %>%
  mutate(ano = gsub("gasto_mensal_familiar_", "", ano))  # Ajustar o nome do ano

# Calcular o maior valor de 'gasto' para cada estado e adicionar 20%
dados_limite <- dados_long_barras %>%
  group_by(nome_estado) %>%
  summarise(max_gasto = max(gasto, na.rm = TRUE)) %>%
  mutate(limite_superior = max_gasto * 1.2)  # 20% acima do maior valor

# Criar nova variável concatenando 'nome_estado' e 'legenda_nivel'
dados_long_barras <- dados_long_barras %>%
  mutate(estado_nivel = paste(nome_estado, legenda_nivel, sep = " - "))


# Criar o gráfico de barras
p2 <- ggplot(dados_long_barras, aes(x = ano, y = gasto, fill = descricao)) +
  geom_bar(stat = "identity", position = "dodge") +  # Gráfico de barras empilhadas (ou lado a lado)
  geom_text(aes(label = round(gasto, 1)),  # Adicionar rótulos com valores arredondados
            position = position_dodge(width = 0.9),  # Ajustar a posição dos rótulos
            vjust = -0.3,  # Eleva os rótulos acima das barras
            size = 3.5) +  # Ajustar o tamanho da fonte
  scale_fill_manual(values = c("No Domicílio" = "#1f78b4", 
                               "Fora do Domicílio" = "#33a02c")) +  # Ajusta cores
  labs(x = "", 
       y = "Gasto Mensal (R$)", 
       fill = "Categoria") +
  theme_bw(base_size = 14) +  # Tema minimalista
  theme(
    #legend.position = "top",  # Legenda no topo
    axis.text.x = element_text(hjust = 0.5),  # Rotaciona os rótulos do eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centraliza o título
  ) +
  facet_wrap(~ estado_nivel, scales = "free_y") +  # Facet por estado
   scale_y_continuous(
  limits = c(0, NA),  # Limite inferior é 0 e superior será definido dinamicamente
  expand = expansion(mult = c(0, 0.2))  # Expande o limite superior com 20% acima
  ) 
# Visualizar gráfico
p2

# Salvar gráfico
ggsave("grafico_gasto_barras.png", plot = p2, 
       width = 10, height = 6, dpi = 300)



# Criar o gráfico de barras
p3 <- ggplot(dados_long_barras, aes(x = ano, y = gasto, fill = descricao)) +
  geom_bar(stat = "identity", position = "stack") +  # Gráfico de barras empilhadas
  geom_text(aes(label = round(gasto,0)),  # Adicionar rótulos com valores arredondados
            position = position_stack(vjust = 0.5),  # Centralizar os rótulos nas barras empilhadas
            size = 4.0, color = "black") +  # Ajustar tamanho e cor da fonte
  scale_fill_manual(values = c("No Domicílio" = "#1f78b4", 
                               "Fora do Domicílio" = "#33a02c")) +  # Ajusta cores
  labs(x = "", 
       y = "Despesa mensal familiar (R$)", 
       fill = "Tipo") +
  theme_bw(base_size = 14) +  # Tema minimalista
  theme(
    #legend.position = "top",  # Legenda no topo
    axis.text.x = element_text(hjust = 0.5),  # Rotaciona os rótulos do eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centraliza o título
  ) +
  facet_wrap(~ estado_nivel, scales = "free_y") +  # Facet por estado
  scale_y_continuous(
    limits = c(0, NA),  # Limite inferior é 0 e superior será definido dinamicamente
    expand = expansion(mult = c(0, 0.2))  # Expande o limite superior com 20% acima
  ) 
# Visualizar gráfico
p3

# Salvar gráfico
ggsave("grafico_gasto_barras_despesa_mensal_familiar_v3.png", plot = p3, 
       width = 10, height = 6, dpi = 300)


dados_barras_estado <- dados_filtrados %>%
  filter(legenda_nivel %in% c("Estado"))

# Reestruturar os dados para formato longo
dados_long_barras_estado <- dados_barras_estado %>%
  pivot_longer(cols = starts_with("gasto_mensal_familiar"), 
               names_to = "ano", 
               values_to = "gasto") %>%
  mutate(ano = gsub("gasto_mensal_familiar_", "", ano))  # Ajustar o nome do ano

# Calcular o maior valor de 'gasto' para cada estado e adicionar 20%
dados_limite_estado <- dados_long_barras_estado %>%
  group_by(nome_estado) %>%
  summarise(max_gasto = max(gasto, na.rm = TRUE)) %>%
  mutate(limite_superior = max_gasto * 1.2)  # 20% acima do maior valor

# Criar nova variável concatenando 'nome_estado' e 'legenda_nivel'
dados_long_barras_estado <- dados_long_barras_estado %>%
  mutate(estado_nivel = paste(nome_estado, legenda_nivel, sep = " - "))


# Criar o gráfico de barras
p4 <- ggplot(dados_long_barras_estado, aes(x = ano, y = gasto, fill = descricao)) +
  geom_bar(stat = "identity", position = "stack") +  # Gráfico de barras empilhadas
  geom_text(aes(label = round(gasto,0)),  # Adicionar rótulos com valores arredondados
            position = position_stack(vjust = 0.5),  # Centralizar os rótulos nas barras empilhadas
            size = 4.0, color = "black") +  # Ajustar tamanho e cor da fonte
  scale_fill_manual(values = c("No Domicílio" = "#1f78b4", 
                               "Fora do Domicílio" = "#33a02c")) +  # Ajusta cores
  labs(x = "", 
       y = "Despesa mensal familiar (R$)", 
       fill = "Tipo") +
  theme_bw(base_size = 14) +  # Tema minimalista
  theme(
    #legend.position = "top",  # Legenda no topo
    axis.text.x = element_text(hjust = 0.5),  # Rotaciona os rótulos do eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centraliza o título
  ) +
  facet_wrap(~ estado_nivel, scales = "free_y") +  # Facet por estado
  scale_y_continuous(
    limits = c(0, NA),  # Limite inferior é 0 e superior será definido dinamicamente
    expand = expansion(mult = c(0, 0.2))  # Expande o limite superior com 20% acima
  ) 
# Visualizar gráfico
p4

# Salvar gráfico
ggsave("grafico_gasto_barras_despesa_mensal_familiar_estado.png", plot = p4, 
       width = 10, height = 6, dpi = 300)


# GRAFICO 2: LOCAIS ------------------------------------------------------------------------
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(ggplot2, dplyr, tidyr,stringr)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


# Etapa 1: Leitura dos dados
tabela_aquisicao <- read.csv(paste(pathdir,"tabela_total_alimentos_nova_local_percprod_2018_amzlegal.csv", sep = ""), sep=";")
colnames(tabela_aquisicao)

tabela_aquisicao <- tabela_aquisicao[c(1,2,3,12:18)]
colnames(tabela_aquisicao) <- c("descricao_estrato","Local","cod_local","Bebida Alcoólica","In natura ou minimamente processado","Processado","Ultraprocessado","Ingredientes Culinários","Preparação Culinária","Outros")
# Selecionar os dois estratos
descricao_estrato_selecionados <- c('AC-UCapital', 'AC-UF','AM-CapitalRRM', 'AM-UF',
                                    'AP-CapitalRRM', 'AP-UF','MA-CapitalRRM', 'MA-UF',
                                    'MT-CapitalRRM', 'MT-UF','PA-CapitalRRM', 'PA-UF',
                                    'RO-UCapital', 'RO-UF','RR-UCapital', 'RR-UF',
                                    'TO-UCapital', 'TO-UF')

dados_filtrados <- tabela_aquisicao %>%
  filter(descricao_estrato %in% descricao_estrato_selecionados) %>%
  mutate(estado = str_sub(descricao_estrato, 1, 2),
         nivel = str_sub(descricao_estrato, 4, 14)) %>%
  mutate(nome_estado = recode(estado,
                              AC = "Acre",
                              AM = "Amazonas",
                              AP = "Amapá",
                              MA = "Maranhão",
                              MT = "Mato Grosso",
                              PA = "Pará",
                              RO = "Rondônia",
                              RR = "Roraima",
                              TO = "Tocantins"),
         legenda_nivel = recode(nivel,
                                UCapital= 'Capital',
                                UF= 'Estado',
                                CapitalRRM = 'Região Metropolitana'))


# Etapa 2: Converter para formato longo
dados_long <- dados_filtrados %>%
  pivot_longer(
    cols = c("Bebida Alcoólica", "In natura ou minimamente processado", 
             "Processado", "Ultraprocessado", 
             "Ingredientes Culinários", "Preparação Culinária", "Outros"),
    names_to = "Categoria",
    values_to = "Percentual"
  )

# Etapa 3: Criar o gráfico por descricao_estrato

subset <- dados_long %>% filter(descricao_estrato == "AC-UCapital")

ggplot(subset, aes(x = Local, y = Percentual, fill = Categoria)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribuição Percentual de Aquisição de Alimentos por Estrato",
    x = "Descrição do Estrato",
    y = "Percentual",
    fill = "Categoria de Alimentos"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


ggplot(dados_long, aes(x = Categoria, y = Percentual, fill = Local)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribuição Percentual de Aquisição de Alimentos por Local",
    x = "Categoria de Alimentos",
    y = "Percentual",
    fill = "Local"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )