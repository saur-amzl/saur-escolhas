
# Limpando a area de trabalho
rm(list=ls())

# Instalando os pacotes
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

# Trecho para criar pasta data
pathdir <- paste(path, "data/", sep = "/")

# Verifica se a pasta não existe antes de criar
if (!dir.exists(pathdir)) {
  dir.create(pathdir, showWarnings = TRUE, recursive = FALSE)
}
