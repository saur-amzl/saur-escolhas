# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl,ggplot2)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
graphdir <-  paste(path, "data/graph/", sep = "/")


tabela_aquisicao_br <- read.csv(paste(outdir,"tab_aquisicao_gasto_mensal_familiar_classes_2008_2018_brasil_10marco2025.csv", sep = ""),sep = ";")
tabela_aquisicao_regional <- read.csv(paste(outdir,"tab_aquisicao_gasto_mensal_familiar_classes_2008_2018_regional_10marco2025.csv", sep = ""),sep = ";")

# Brasil - com outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes"))) %>%
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              filter(POF == "2008/2009"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 5.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste0(graphdir,"/gasto_aquisicao_alimdecreto_br_10marco2025.png", sep = ""), 
       plot = p1, 
       width = 10,  # Largura em polegadas
       height = 7,  # Altura em polegadas
       dpi = 300,   # Resolução (dots per inch)
       units = "in") # Unidade para as dimensões (pode ser "in" para polegadas, "cm" para centímetros)


# Brasil - sem outros ----------------------------------------------------------
subset <- tabela_aquisicao_br %>%
  filter(indicador_nivel == "alimentos_decreto_bebidas") %>%
  filter(!(descricao %in% c("Café e Chá", "Castanhas e nozes","Outros"))) %>%
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              filter(POF == "2008/2009"),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 5.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste(graphdir,"/gasto_aquisicao_alimdecreto_br_semoutros_10marco2025.png", sep = ""), 
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
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              group_by(descricao) %>%  # Agrupar por descricao
              filter(Valor == max(Valor)),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 2.8, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste(graphdir,"/gasto_aquisicao_alimdecreto_mediaestados_10marco2025.png", sep = ""), 
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
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              group_by(descricao) %>%  # Agrupar por descricao
              filter(Valor == max(Valor)),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 5.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste(graphdir,"/gasto_aquisicao_alimdecreto_mediaestados_semoutros_10marco2025.png", sep = ""), 
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
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              group_by(descricao) %>%  # Agrupar por descricao
              filter(Valor == max(Valor)),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 5.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste(graphdir,"/gasto_aquisicao_alimdecreto_mediacapitais_10marco2025.png", sep = ""), 
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
  mutate(perc = gasto_mensal_familiar_2018 / gasto_mensal_familiar_2008 - 1) %>%
  rename(`2008/2009` = gasto_mensal_familiar_2008,
         `2017/2018` = gasto_mensal_familiar_2018) %>%
  select(descricao, `2008/2009`, `2017/2018`, perc)

# Transformando os dados para formato long
subset_long <- subset %>%
  pivot_longer(cols = c(`2008/2009`, `2017/2018`), 
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
              group_by(descricao) %>%  # Agrupar por descricao
              filter(Valor == max(Valor)),
            aes(x = factor(descricao, levels = my_order_descr), 
                y = Valor + 7.5, 
                label = scales::percent(perc, accuracy = 0.1)),
            color = "black", size = 3.5,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = new_names) + # Alterando os nomes
  scale_fill_manual(values = c("2008/2009" = "#D1A1D6", "2017/2018" = "#8A4E98")) +  # Cores roxo clarinho e roxo escuro
  labs(x = "", y = "Gasto mensal familiar (R$)",
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
ggsave(paste(graphdir,"/gasto_aquisicao_alimdecreto_mediacapitais_semoutros_10marco2025.png", sep = ""), 
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

