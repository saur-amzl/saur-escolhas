# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl,ggplot2)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


tabela_aquisicao_br <- read.csv(paste(pathdir,"tabela_aquisicao_2008_2018_brasil_16fev2025.csv", sep = ""),sep = ";")
tabela_aquisicao_regional <- read.csv(paste(pathdir,"tabela_aquisicao_2008_2018_regional_16fev2025.csv", sep = ""),sep = ";")


# Brasil - com outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb",
                   "Outros")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas",
               "Outros" = "Outros")


# Criando o gráfico
p1 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_br.png", sep = ""), 
       plot = p1, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


# Brasil - sem outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes","Outros"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas")


# Criando o gráfico
p2 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_br_semoutros.png", sep = ""), 
       plot = p2, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)






# Média Estado - com outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-UF") %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb",
                   "Outros")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas",
               "Outros" = "Outros")


# Criando o gráfico
p3 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_media_estados.png", sep = ""), 
       plot = p3, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)



# Média Estado - sem outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-UF") %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes","Outros"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas")


# Criando o gráfico
p4 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_media_estados_semoutros.png", sep = ""), 
       plot = p4, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


# Média Capital - com outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-Capital") %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb",
                   "Outros")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas",
               "Outros" = "Outros")


# Criando o gráfico
p5 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_media_capitais.png", sep = ""), 
       plot = p5, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)

# Média Capital - sem outros -----------------------
rm(subset,subset_long)
subset <- tabela_aquisicao_regional %>%
  filter(nivel == "Média-Capital") %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes","Outros"))) %>%
  mutate(perc = qtd_anual_percapita_2018 / qtd_anual_percapita_2008 - 1) %>%
  rename(`2007/2008` = qtd_anual_percapita_2008,
         `2017/2018` = qtd_anual_percapita_2018) %>%
  select(descricao, `2007/2008`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2007/2008`, `2017/2018`), 
               names_to = "POF", values_to = "Valor")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Ultraprocessados",
                   "Ultraprocessados_beb")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas")


# Criando o gráfico
p6 <- ggplot(subset_long, aes(x = factor(descricao, levels = my_order_descr), y = Valor, fill = POF)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(data = subset_long %>% 
              filter(POF == "2007/2008"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 1.5, 
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
ggsave(paste(pathdir,"grafico_comparacao_aquisicao_media_capitais_semoutros.png", sep = ""), 
       plot = p6, 
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

