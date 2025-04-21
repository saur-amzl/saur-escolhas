
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(ggplot2, dplyr, tidyr,stringr)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs", sep = "/")



# GRAFICO 1: GASTOS ---------------------------------------------------
tabela_gastos <- read.csv(paste(outdir,"/tabela_gastos_aquisicao_2008_2018_brasil_27fev2025.csv", sep = ""), sep=";")


data_filtered <- tabela_gastos %>% filter(	
  indicador_nivel ==  "alimentos_decreto_bebidas")


my_order_descr = c("Leguminosa",
                   "Cereais",
                   "Tubérculos e raízes",
                   "Legumes e verduras",
                   "Frutas",
                   "Carnes e Ovos",
                   "Leites e Queijos",
                   "Farinha",
                   "Castanhas e nozes",
                   "Café e Chá",
                   "Ultraprocessados",
                   "Ultraprocessados_beb",
                   "Outros")

my_colors <- c(
  "Leguminosa" = "#8C510A",        # Marrom terroso
  "Cereais" = "#D8B365",           # Bege claro
  "Tubérculos e raízes" = "#A6761D", # Marrom alaranjado
  "Legumes e verduras" = "#66A61E",  # Verde vibrante
  "Frutas" = "#E7298A",            # Rosa intenso
  "Carnes e Ovos" = "#E63946",      # Vermelho carne
  "Leites e Queijos" = "#F4E04D",   # Amarelo queijo
  "Farinha" = "#F4A261",           # Laranja claro
  "Castanhas e nozes" = "#D9A066",  # Marrom dourado
  "Café e Chá" = "#6F4E37",        # Marrom café
  "Ultraprocessados" = "#1F78B4",  # Azul vibrante
  "Ultraprocessados_beb" = "#33A9FF", # Azul claro
  "Outros" = "#999999"             # Cinza neutro
)

# Transformar os dados para o formato longo
data_long <- data_filtered %>%
  pivot_longer(cols = starts_with("gasto_mensal_familiar"), 
               names_to = "ano", 
               values_to = "gasto") %>%
  mutate(ano = ifelse(ano == "gasto_mensal_familiar_2008", "2007/2008", "2017/2018"),
         descricao = factor(descricao, levels = my_order_descr))  # Aplicar ordem


# Calcular percentual por ano
data_long <- data_long %>%
  group_by(ano) %>%
  mutate(percentual = (gasto / sum(gasto)) * 100) %>%
  ungroup()

# Garantir a ordem correta das categorias
data_long$descricao <- factor(data_long$descricao, levels = my_order_descr)

# Definir cores personalizadas
my_colors <- c("Leguminosa" = "#8B4513", 
               "Cereais" = "#D2B48C", 
               "Tubérculos e raízes" = "#FF6347", 
               "Legumes e verduras" = "#228B22", 
               "Frutas" = "#FF1493", 
               "Carnes e Ovos" = "#DC143C", 
               "Leites e Queijos" = "#FFD700", 
               "Farinha" = "#FFA07A", 
               "Castanhas e nozes" = "#8B4513", 
               "Café e Chá" = "#5F4B32", 
               "Ultraprocessados" = "#1E90FF", 
               "Ultraprocessados_beb" = "#87CEFA", 
               "Outros" = "#A9A9A9")

# Criar gráfico de pizza
ggplot(data_long, aes(x = "", y = percentual, fill = descricao)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ano, ncol = 2) +
  scale_fill_manual(values = my_colors) +
  labs(title = "",
       fill = "Categoria") +
  theme_void() + 
  theme(legend.position = "bottom")

# Definindo os novos nomes
new_names <- c("Leguminosa" = "Leguminosas",
               "Cereais" = "Cereais",
               "Tubérculos e raízes" = "Raízes e \nTubérculos",
               "Legumes e verduras" = "Legumes \n e Verduras",
               "Frutas" = "Frutas",
               "Carnes e Ovos" = "Carnes\n e Ovos",
               "Leites e Queijos" = "Leites \n e Queijos",
               "Farinha" = "Farinha",
               "Castanhas e nozes" = "Castanhas \n e nozes",
               "Café e Chá" = "Café \n e Chá",
               "Ultraprocessados" = "Ultra-\n processados",
               "Ultraprocessados_beb" = "Bebidas \n Ultra-\n processadas",
               "Outros" = "Outros")

data_long$descricao <- factor(data_long$descricao, levels = my_order_descr)
data_long %>% group_by(ano) %>% summarise(total = sum(gasto))


# Criar gráfico de pizza para cada ano
ggplot(data_long, aes(x = "", y = gasto, fill = descricao)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ ano) +  # Criar um gráfico para cada ano
  theme_void() +  # Remover eixos
  labs(title = "Distribuição dos Gastos com Alimentos (2008 vs 2018)", fill = "Categoria") +
  scale_fill_manual(values = my_colors) +  # Aplicar cores personalizadas
  theme(legend.position = "bottom")

