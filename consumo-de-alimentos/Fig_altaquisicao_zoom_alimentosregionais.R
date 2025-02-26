# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, tidyr,RColorBrewer,readxl,ggplot2)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


tabela_aquisicao_br <- read.csv(paste(pathdir,"tabela_aquisicao_2008_2018_brasil_16fev2025.csv", sep = ""),sep = ";")
tabela_aquisicao_regional <- read.csv(paste(pathdir,"tabela_aquisicao_2008_2018_regional_16fev2025.csv", sep = ""),sep = ";")

# Brasil - com outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_regionais") %>%
  filter(descricao %in% c("Açaí","Banana-nanica","Banana-da-terra","Banana-pacova","Cebola","Cupuaçu","Feijão","Feijão-de-corda","Feijão-verde","Mandioca","Mamão","Milho-verde","Pescados")) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")

my_order_descr = c("Açaí",
                   "Banana-nanica",
                   "Banana-da-terra",
                   "Banana-pacova",
                   "Cebola",
                   "Cupuaçu",
                   "Feijão",
                   "Feijão-de-corda",
                   "Feijão-verde",
                   "Mandioca",
                   "Mamão",
                   "Milho-verde",
                   "Pescados")

# Definindo os novos nomes
new_names = c("Açaí" = "Açaí",
                   "Banana-nanica" = "Banana\n-nanica",
                   "Banana-da-terra" ="Banana\n-da-terra",
                   "Banana-pacova" = "Banana\n-pacova",
                   "Cebola" = "Cebola",
                   "Cupuaçu" = "Cupuaçu",
                   "Feijão" = "Feijão",
                   "Feijão-de-corda"= "Feijão\n-de-corda",
                   "Feijão-verde" = "Feijão\n-verde",
                   "Mandioca" =  "Mandioca",
                   "Mamão" = "Mamão",
                   "Milho-verde" = "Milho\n-verde",
                   "Pescados" = "Pescados")

# Criando o gráfico
p1 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.3, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2007/2008" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12))  # Aumentando a fonte do texto da legenda

p1
# Salvar o gráfico como .png
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_zoomregionais_br.png", sep = ""), 
       plot = p1, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


# Média Estado - com outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-UF") %>%
  filter(descricao %in% c("Açaí","Banana-nanica","Banana-da-terra","Banana-pacova","Cebola","Cupuaçu","Feijão","Feijão-de-corda","Feijão-verde","Mandioca","Mamão","Milho-verde","Pescados")) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")

my_order_descr = c("Açaí",
                   "Banana-nanica",
                   "Banana-da-terra",
                   "Banana-pacova",
                   "Cebola",
                   "Cupuaçu",
                   "Feijão",
                   "Feijão-de-corda",
                   "Feijão-verde",
                   "Mandioca",
                   "Mamão",
                   "Milho-verde",
                   "Pescados")

# Definindo os novos nomes
new_names = c("Açaí" = "Açaí",
              "Banana-nanica" = "Banana\n-nanica",
              "Banana-da-terra" ="Banana\n-da-terra",
              "Banana-pacova" = "Banana\n-pacova",
              "Cebola" = "Cebola",
              "Cupuaçu" = "Cupuaçu",
              "Feijão" = "Feijão",
              "Feijão-de-corda"= "Feijão\n-de-corda",
              "Feijão-verde" = "Feijão\n-verde",
              "Mandioca" =  "Mandioca",
              "Mamão" = "Mamão",
              "Milho-verde" = "Milho\n-verde",
              "Pescados" = "Pescados")


# Criando o gráfico
p2 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2007/2008" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12))  # Aumentando a fonte do texto da legenda


# Salvar o gráfico como .png
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_zoomregionais_media_estados.png", sep = ""), 
       plot = p2, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


# Média Capital - com outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-Capital") %>%
  filter(descricao %in% c("Açaí","Banana-nanica","Banana-da-terra","Banana-pacova","Cebola","Cupuaçu","Feijão","Feijão-de-corda","Feijão-verde","Mandioca","Mamão","Milho-verde","Pescados")) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")

my_order_descr = c("Açaí",
                   "Banana-nanica",
                   "Banana-da-terra",
                   "Banana-pacova",
                   "Cebola",
                   "Cupuaçu",
                   "Feijão",
                   "Feijão-de-corda",
                   "Feijão-verde",
                   "Mandioca",
                   "Mamão",
                   "Milho-verde",
                   "Pescados")

# Definindo os novos nomes
new_names = c("Açaí" = "Açaí",
              "Banana-nanica" = "Banana\n-nanica",
              "Banana-da-terra" ="Banana\n-da-terra",
              "Banana-pacova" = "Banana\n-pacova",
              "Cebola" = "Cebola",
              "Cupuaçu" = "Cupuaçu",
              "Feijão" = "Feijão",
              "Feijão-de-corda"= "Feijão\n-de-corda",
              "Feijão-verde" = "Feijão\n-verde",
              "Mandioca" =  "Mandioca",
              "Mamão" = "Mamão",
              "Milho-verde" = "Milho\n-verde",
              "Pescados" = "Pescados")


# Criando o gráfico
p3 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2007/2008" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12))  # Aumentando a fonte do texto da legenda


# Salvar o gráfico como .png
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_zoomregionais_media_capitais.png", sep = ""), 
       plot = p3, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


##-----------------

#legend.position = c(0.85, 0.85),  # Posição da legenda dentro do gráfico (x, y) - ajustando para o canto superior direito
#legend.justification = c("right", "top"),  # Justificando a legenda no canto superior direito
#legend.box.just = "center",  # Centralizando a caixa da legenda dentro do gráfico
#legend.title = element_text(size = 14),  # Aumentando a fonte do título da legenda
#legend.text = element_text(size = 12))  # Aumentando a fonte do texto da legenda

