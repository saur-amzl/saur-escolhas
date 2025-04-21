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
outdir <- paste(path, "data/outputs/", sep = "/")
graphdir <-  paste(path, "data/graph/", sep = "/")



# Etapa 1: Leitura dos dados ----------------------------------------------------
seg_alimentar_all <- read.csv(paste(outdir,"tabela_seguranca_alimentar_numtotal_15marco2025.csv", sep = ""),sep = ";")


# Alterando o nome da pesquisa POF 
seg_alimentar_all <- seg_alimentar_all %>%
  mutate(fonte = ifelse(fonte == "POF 2018", "POF 2017/2018", fonte))


#Etapa 2: Criando Graficos -------------------------------------------------------------

# Criando a ordem correta das categorias de segurança alimentar
ordem_niveis <- c("Segurança Alimentar", "Insegurança Alimentar Leve", 
                  "Insegurança Alimentar Moderada", "Insegurança Alimentar Grave")

# Criando a ordem correta das fontes
ordem_fontes <- c("PNAD 2004", "PNAD 2009", "PNAD 2013", "POF 2017/2018", "PNADC 2023")

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
      legend.position = "bottom",  # Coloca a legenda na parte inferior
      panel.background = element_rect(fill = "white", color = NA),  # Fundo interno branco
      plot.background = element_rect(fill = "transparent", color = NA), # Fundo externo transparente
      legend.background = element_rect(fill = "transparent", color = NA), # Fundo da legenda transparente
      legend.key = element_rect(fill = "transparent", color = NA)
    ) 
  
  # Exporta o gráfico para um arquivo PNG
  ggsave(filename = paste0(graphdir,"/seguranca_alimentar_urbano_", gsub(" ", "_", regiao), "_15marco.png"),
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
    legend.position = "bottom",  # Coloca a legenda na parte inferior
    panel.background = element_rect(fill = "white", color = NA),  # Fundo interno branco
    plot.background = element_rect(fill = "transparent", color = NA), # Fundo externo transparente
    legend.background = element_rect(fill = "transparent", color = NA), # Fundo da legenda transparente
    legend.key = element_rect(fill = "transparent", color = NA)
  )  

# Exporta o gráfico para um arquivo PNG
ggsave(filename = paste0(graphdir,"seguranca_alimentar_urbano_todosufamzl", "_15marco.png"),
       plot = grafico, width = 8, height = 6, dpi = 300)
