
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

# Autenticando o google drive do projeto
# O comando abaixo vai te direcionar pra uma pagina no navegador onde voce deve conectar 
# na conta autorizada a acessar a pasta do projeto. Neste processo, voce deve selecionar a permissao
# para o pacote tidyverse ver, editar, excluir, modificar, etc., os arquivos do seu drive.
#drive_auth(cache = FALSE, scopes = "https://www.googleapis.com/auth/drive") 

# Download dos dados
#file <- drive_get("tis_poligonais_portarias.csv")
#drive_download(file, path = paste(pathdir, "tis_poligonais_portarias.csv", sep = "/"))

#file <- drive_get("tis_pontos_portarias.csv")
#drive_download(file, path = paste(pathdir, "tis_pontos_portarias.csv", sep = "/"))

#file <- drive_get("aldeias_pontos.csv")
#drive_download(file, path = paste(pathdir, "aldeias_pontos.csv", sep = "/"))

#file <- drive_get("tis_poligonais.csv")
#drive_download(file, path = paste(pathdir, "tis_poligonais.csv", sep = "/"))

#file <- drive_get("atributos_estado_regiao.csv")
#drive_download(file, path = paste(pathdir, "atributos_estado_regiao.csv", sep = "/"))

#file <- drive_get("apendice_01_Pessoas_residentes_TI_Brasil_2010_e_2022_20231222.xlsx")
#drive_download(file, path = paste(pathdir, "apendice_01_Pessoas_residentes_TI_Brasil_2010_e_2022_20231222.xlsx", sep = "/"))

#file <- drive_get("censo2022_sidra_9718_PessInd_munic_22_pol.csv")
#drive_download(file, path = paste(pathdir, "censo2022_sidra_9718_PessInd_munic_22_pol.csv", sep = "/"))
