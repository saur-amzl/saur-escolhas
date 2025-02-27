# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table,readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)  # Carrega o pacote
library(tidyr)
library(openxlsx)
library(ggplot2)




# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
inputdir <- paste(path, "data/inputs/", sep = "/")
outdir <- paste(path, "data/outputs/", sep = "/")

# Etapa 1: Leitura dos dados ----------------------------------------------------
pnad_2004 <- read.csv(paste0(outdir,"tabela_seguranca_alimentar_pnad_2004_26marco2025.csv"),sep = ";")
pnad_2009 <- read.csv(paste0(outdir,"tabela_seguranca_alimentar_pnad_2009_26marco2025.csv"),sep = ";")
pnad_2013 <- read.csv(paste0(outdir,"tabela_seguranca_alimentar_pnad_2013_26marco2025.csv"),sep = ";")
pof_2018 <- read.csv(paste0(outdir,"tabela_seguranca_alimentar_pof_2018_26marco2025.csv"),sep = ";")
pnadc_2023 <- read.csv(paste0(outdir,"tabela_seguranca_alimentar_pnadc_2023_26marco2025.csv"),sep = ";")

# Adicionando a coluna 'fonte' em cada base de dados
pnad_2004 <- pnad_2004 %>% mutate(fonte = "PNAD 2004") %>% filter(seguranca_alimentar != "Não Aplicável")
pnad_2009 <- pnad_2009 %>% mutate(fonte = "PNAD 2009") %>% filter(seguranca_alimentar != "Não Aplicável")
pnad_2013 <- pnad_2013 %>% mutate(fonte = "PNAD 2013") %>% filter(seguranca_alimentar != "Não Aplicável")
pof_2018 <- pof_2018 %>% mutate(fonte = "POF 2018")
pnadc_2023 <- pnadc_2023 %>% mutate(fonte = "PNADC 2023")

# Unindo todas as bases
seg_alimentar_all <- bind_rows(pnad_2004, pnad_2009, pnad_2013, pof_2018, pnadc_2023)

# Seleciona os dados de interesse
seg_alimentar_all <- seg_alimentar_all %>% 
  filter(Regiao %in% c("Brasil","Rondônia","Acre","Amazonas",
                                                  "Roraima","Pará","Amapá","Tocantins",
                                                  "Maranhão","Mato Grosso")) %>% 
  select(fonte,Regiao,seguranca_alimentar,situacao_domicilio,num_domicilios)


# Adiciona Situação de Domicílio TOTAL (com sumarização entre Urbano e Rural)
# Criar linha de total por segurança alimentar
totais_situacao <- seg_alimentar_all %>%
  group_by(fonte, Regiao, seguranca_alimentar) %>%
  summarise(num_domicilios = sum(num_domicilios), .groups = "drop") %>%
  mutate(situacao_domicilio = "Total")

# Unir os totais ao conjunto original
seg_alimentar_all <- bind_rows(seg_alimentar_all, totais_situacao) %>%
  arrange(seguranca_alimentar, situacao_domicilio)

# Calculando os totais considerando todas as categorias de segurança alimentar,
# para calcular as porcentagens
totais <- seg_alimentar_all %>%
  group_by(fonte,Regiao,situacao_domicilio) %>%
  summarise(
    total_domicilios = sum(num_domicilios, na.rm = TRUE)
  )

# Unindo os totais com os dados principais
dados_completos <- seg_alimentar_all %>%
  left_join(totais, by = c('fonte','Regiao',"situacao_domicilio")) %>%
  mutate(
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

# Criando a linha de Insegurança Alimentar Total
inseguranca_total <- dados_completos %>%
  filter(grepl("Insegurança Alimentar", seguranca_alimentar)) %>%
  group_by(fonte,Regiao,situacao_domicilio) %>%
  summarise(
    seguranca_alimentar = "Insegurança Alimentar",
    num_domicilios = sum(num_domicilios),
    total_domicilios = first(total_domicilios)
  ) %>%
  mutate(
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

dados_completos_final <- bind_rows(dados_completos, inseguranca_total)

# ETAPA 3: EXPORTA TABELA FINAL
# Definir a ordem desejada para a coluna seguranca_alimentar
ordem_seg_alim <- c("Segurança Alimentar", "Insegurança Alimentar", 
                    "Insegurança Alimentar Leve", "Insegurança Alimentar Moderada", 
                    "Insegurança Alimentar Grave")

# Criar uma lista de regiões
regioes <- unique(dados_completos_final$Regiao)

# Criar um workbook do Excel
wb <- createWorkbook()

for (regiao in regioes) {
  # Filtrar os dados para a região específica
  dados_regiao <- dados_completos_final %>%
    filter(Regiao == regiao) %>%
    select(fonte, seguranca_alimentar, situacao_domicilio, percentual_domicilios) %>%
    pivot_wider(names_from = situacao_domicilio, values_from = percentual_domicilios, values_fill = 0) %>%
    mutate(seguranca_alimentar = factor(seguranca_alimentar, levels = ordem_seg_alim)) %>%
    arrange(seguranca_alimentar) %>%
    select(fonte, seguranca_alimentar, Total, Urbano, Rural)  # Ordenação correta das colunas
  
  # Renomear colunas para ficarem mais claras
  colnames(dados_regiao) <- c("Fonte", "Situação de Segurança Alimentar", "Total", "Urbano", "Rural")
  
  # Criar uma aba para a região no Excel
  addWorksheet(wb, regiao)
  
  # Escrever os dados na aba
  writeData(wb, regiao, dados_regiao)
}

# Salvar o arquivo Excel
saveWorkbook(wb,paste0(outdir,"tabela_seguranca_alimentar_domicilio_27marco2025.xlsx"), overwrite = TRUE)

cat("Arquivo Excel gerado com sucesso!\n")

#Etapa 4: Graficos -------------------------------------------------------------

# Criando a ordem correta das categorias de segurança alimentar
ordem_niveis <- c("Segurança Alimentar", "Insegurança Alimentar Leve", 
                  "Insegurança Alimentar Moderada", "Insegurança Alimentar Grave")

# Criando a ordem correta das fontes
ordem_fontes <- c("PNAD 2004", "PNAD 2009", "PNAD 2013", "POF 2018", "PNADC 2023")

graph <- seg_alimentar_all %>%
  filter(situacao_domicilio == "Urbano") %>%  # Filtra apenas os dados urbanos
  mutate(
    seguranca_alimentar = factor(seguranca_alimentar, levels = ordem_niveis),
    fonte = factor(fonte, levels = ordem_fontes)  # Garante a ordem correta das fontes
  )

# Criar os gráficos e exportar um por região
lista_regioes <- unique(graph$Regiao)

for (regiao in lista_regioes) {
  # Filtra a região
  dados_regiao <- graph %>% filter(Regiao == regiao)
  
  # Calcula os percentuais dentro de cada fonte e nível de segurança alimentar
  dados_regiao <- dados_regiao %>%
    group_by(fonte, seguranca_alimentar) %>%
    summarise(num_domicilios = sum(num_domicilios, na.rm = TRUE), .groups = "drop") %>%
    group_by(fonte) %>%
    mutate(percentual = num_domicilios / sum(num_domicilios))  # Calcula o percentual
  
  # Cria o gráfico
  grafico <- ggplot(dados_regiao, aes(x = seguranca_alimentar, 
                                      y = percentual, 
                                      fill = fonte)) +
    geom_bar(stat = "identity", position = "dodge") +  # Barras lado a lado
    geom_text(aes(label = 100*round(percentual, 3)),
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Adiciona percentuais
    scale_fill_manual(values = c("#144869", "#1c6694", "#2484be", "#3c9eda", "#7cbee6")) +  # Cores personalizadas
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +  # Formata eixo Y como percentual
    scale_x_discrete(labels = c(
      "Segurança Alimentar" = "Segurança\nAlimentar",
      "Insegurança Alimentar Leve" = "Insegurança\nAlimentar Leve",
      "Insegurança Alimentar Moderada" = "Insegurança\nAlimentar Moderada",
      "Insegurança Alimentar Grave" = "Insegurança\nAlimentar Grave"
    )) + 
    labs(x = "", 
         y = "", 
         fill = "",
         title = paste(regiao)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  # Centraliza as labels
      axis.title.x = element_text(hjust = 0.5),  # Centraliza o título do eixo X
      legend.position = "bottom"  # Coloca a legenda na parte inferior
    )  # Inclina os rótulos do eixo X
  
  # Exporta o gráfico para um arquivo PNG
  ggsave(filename = paste0(outdir,"/seguranca_alimentar_urbano_", gsub(" ", "_", regiao), ".png"),
         plot = grafico, width = 8, height = 6, dpi = 300)
}


## Todos os dados da REGIÃO
dados_regiao <- graph %>% filter(Regiao != "Brasil")

# Calcula os percentuais dentro de cada fonte e nível de segurança alimentar
dados_regiao <- dados_regiao %>%
  group_by(fonte, seguranca_alimentar) %>%
  summarise(num_domicilios = sum(num_domicilios, na.rm = TRUE), .groups = "drop") %>%
  group_by(fonte) %>%
  mutate(percentual = num_domicilios / sum(num_domicilios))  # Calcula o percentual

# Cria o gráfico
grafico <- ggplot(dados_regiao, aes(x = seguranca_alimentar, 
                                    y = percentual, 
                                    fill = fonte)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras lado a lado
  geom_text(aes(label = 100*round(percentual, 3)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Adiciona percentuais
  scale_fill_manual(values = c("#144869", "#1c6694", "#2484be", "#3c9eda", "#7cbee6")) +  # Cores personalizadas
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +  # Formata eixo Y como percentual
  scale_x_discrete(labels = c(
    "Segurança Alimentar" = "Segurança\nAlimentar",
    "Insegurança Alimentar Leve" = "Insegurança\nAlimentar Leve",
    "Insegurança Alimentar Moderada" = "Insegurança\nAlimentar Moderada",
    "Insegurança Alimentar Grave" = "Insegurança\nAlimentar Grave"
  )) + 
  labs(x = "", 
       y = "", 
       fill = "",
       title = "Estados da Amazônia Legal") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  # Centraliza as labels
    axis.title.x = element_text(hjust = 0.5),  # Centraliza o título do eixo X
    legend.position = "bottom"  # Coloca a legenda na parte inferior
  )  # Inclina os rótulos do eixo X

# Exporta o gráfico para um arquivo PNG
ggsave(filename = paste0(outdir,"seguranca_alimentar_urbano_todosufamzl", ".png"),
       plot = grafico, width = 8, height = 6, dpi = 300)
