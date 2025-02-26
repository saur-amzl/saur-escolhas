# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table,readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
inputdir <- paste(path, "data/inputs/", sep = "/")

library("PNADcIBGE")
#https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html?caminho=Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre
pnadc2023 <- read_pnadc(microdata = "/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/PNADC/PNADC_2023_trimestre4.txt",
           input_txt = "/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/inputs/input_PNADC_trimestre4_20240425.txt")

dados_pnadc <- pnadc_labeller(pnadc2023, "/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/PNADC/dicionario_PNADC_microdados_trimestre4_20240816.xls")


confere <- dados_pnadc %>% 
  select(UF, Capital, RM_RIDE) %>% 
  distinct()

#V1022		Situação do domicílio	1	Urbana 2	Rural
#V1027		Peso do domicílio e das pessoas
#V1028		Peso do domicílio e das pessoas
#V2001	1	Número de pessoas no domicílio
#21    	7	Estrato

#SD17001		Situação de segurança alimentar existente no domicílio	
#1	Segurança alimentar	4º tri/2023
#2	Insegurança alimentar leve	
#3	Insegurança alimentar moderada	
#4	Insegurança alimentar grave 	
# Não aplicável	
# Criar uma nova variável categórica para facilitar a análise

aa <- pnadc2023 %>% select(SD17001,V1028,V2001,V1022)

pnadc2023 <- pnadc2023 %>%
  mutate(seguranca_alimentar = case_when(
    SD17001 %in% c(1) ~ "Segurança Alimentar",
    SD17001 %in% c(2) ~ "Insegurança Alimentar Leve",
    SD17001 %in% c(3) ~ "Insegurança Alimentar Moderada",
    SD17001 %in% c(4) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

pnadc2023 <- pnadc2023 %>%
  mutate(situacao_domicilio = case_when(
    V1022 %in% c(1) ~ "Urbano",
    V1022 %in% c(2) ~ "Rural"
  ))


sumarizacao_segalimentar <- pnadc2023 %>%
  group_by(seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_peso_pessoas_domicilios = sum(V1028, na.rm=TRUE), # peso do domicilio e das pessoas
            num_domicilios = sum(V1028/V2001,na.rm=TRUE))  # peso do domicilio e das pessoas / num de pessoas no domicilio
          


