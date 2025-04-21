# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, tidyr,RColorBrewer,readxl,ggplot2)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
graphdir <-  paste(path, "data/graph/", sep = "/")


tabela_aquisicao_br <- read.csv(paste(outdir,"tab_aquisicao_qtidade_percapita_classes_2008_2018_brasil_10marco2025.csv", sep = ""),sep = ";")
tabela_aquisicao_regional <- read.csv(paste(outdir,"tab_aquisicao_qtidade_percapita_classes_2008_2018_regional_10marco2025.csv", sep = ""),sep = ";")

# Brasil - com outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_regionais") %>%
  filter(descricao %in% c("Açaí","Banana-nanica","Banana-da-terra","Banana-pacova","Cebola","Cupuaçu","Feijão","Feijão-de-corda","Feijão-verde","Mandioca","Mamão","Milho-verde","Pescados")) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2008/2009` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              filter(POF == "2008/2009"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.3, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12),  # Aumentando a fonte do texto da legenda
        panel.background = element_rect(fill = "white", color = NA),  # Fundo interno branco
        plot.background = element_rect(fill = "transparent", color = NA), # Fundo externo transparente
        legend.background = element_rect(fill = "transparent", color = NA), # Fundo da legenda transparente
        legend.key = element_rect(fill = "transparent", color = NA)) # Fundo das chaves da legenda transparente

p1
# Salvar o gráfico como .png
ggsave(paste(graphdir,"qtidade_aquisicao_zoomregionais_br_10marco2025.png", sep = ""), 
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
  rename(`2008/2009` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              filter(POF == "2008/2009"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12),  # Aumentando a fonte do texto da legenda
        panel.background = element_rect(fill = "white", color = NA),  # Fundo interno branco
        plot.background = element_rect(fill = "transparent", color = NA), # Fundo externo transparente
        legend.background = element_rect(fill = "transparent", color = NA), # Fundo da legenda transparente
        legend.key = element_rect(fill = "transparent", color = NA)) # Fundo das chaves da legenda transparente


# Salvar o gráfico como .png
ggsave(paste(graphdir,"qtidade_aquisicao_zoomregionais_mediaestados_10marco2025.png", sep = ""), 
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
  rename(`2008/2009` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)


# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              filter(POF == "2008/2009"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 0.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Aquisição per capita anual (kg)",
       fill = "POF") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),  # Aumentando a fonte do eixo X
        axis.text.y = element_text(size = 12),  # Aumentando a fonte do eixo Y
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",  # Colocando a legenda no topo
        legend.title = element_text(size = 12),  # Aumentando a fonte do título da legenda
        legend.text = element_text(size = 12),  # Aumentando a fonte do texto da legenda
        panel.background = element_rect(fill = "white", color = NA),  # Fundo interno branco
        plot.background = element_rect(fill = "transparent", color = NA), # Fundo externo transparente
        legend.background = element_rect(fill = "transparent", color = NA), # Fundo da legenda transparente
        legend.key = element_rect(fill = "transparent", color = NA)) # Fundo das chaves da legenda transparente


# Salvar o gráfico como .png
ggsave(paste(graphdir,"qtidade_aquisicao_zoomregionais_mediacapitais_10marco2025.png", sep = ""), 
       plot = p3, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)

