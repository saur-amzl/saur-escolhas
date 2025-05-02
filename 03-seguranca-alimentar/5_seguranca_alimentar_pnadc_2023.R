# ------------------------------------------------------------------------------
# Script para calcular segurando alimentar da PNADC 2023
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
library("PNADcIBGE")

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/raw/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")
intdir <-  paste(path, "data/intermediate/", sep = "/")


# Etapa 1: Leitura dos dados ----------------------------------------------------
#https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html?caminho=Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre
pnadc_2023 <- read_pnadc(microdata = paste0(pathdir,"PNADC/PNADC_2023_trimestre4.txt"),
           input_txt = paste0(dicdir,"input_PNADC_trimestre4_20240425.txt"))

#dados_pnadc <- pnadc_labeller(pnadc_2023, "/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/PNADC/dicionario_PNADC_microdados_trimestre4_20240816.xls")

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

pnadc_2023 <- pnadc_2023 %>%
  mutate(seguranca_alimentar = case_when(
    SD17001 %in% c(1) ~ "Segurança Alimentar",
    SD17001 %in% c(2) ~ "Insegurança Alimentar Leve",
    SD17001 %in% c(3) ~ "Insegurança Alimentar Moderada",
    SD17001 %in% c(4) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

pnadc_2023 <- pnadc_2023 %>%
  mutate(situacao_domicilio = case_when(
    V1022 %in% c(1) ~ "Urbano",
    V1022 %in% c(2) ~ "Rural"
  ))


sumarizacao_segalimentar <- pnadc_2023 %>%
  group_by(seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_peso_pessoas_domicilios = sum(V1028, na.rm=TRUE), # peso do domicilio e das pessoas
            num_domicilios = sum(V1028/V2001,na.rm=TRUE))  # peso do domicilio e das pessoas / num de pessoas no domicilio
          


# Gerando o dado para o Brasil
pnadc_2023_brasil <- pnadc_2023 %>%
  mutate(Regiao = "Brasil") %>%
  group_by(Regiao, seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V1028, na.rm=TRUE), # peso do domicilio e das pessoas
            num_domicilios = sum(V1028/V2001, na.rm = TRUE)) # peso do domicilio e das pessoas / num de pessoas no domicilio


# Gerando o dado para os estados
pnadc_2023_estados <- pnadc_2023 %>%
  group_by(UF,seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V1028, na.rm=TRUE), # peso do domicilio e das pessoas
            num_domicilios = sum(V1028/V2001, na.rm = TRUE)) %>% # peso do domicilio e das pessoas / num de pessoas no domicilio
  rename(Regiao = UF)

# Juntando os dois dataframes
pnadc_2023_completo <- bind_rows(pnadc_2023_brasil, pnadc_2023_estados)

# Substituindo os códigos pelos nomes dos estados usando case_when
pnadc_2023_completo <- pnadc_2023_completo %>%
  mutate(Regiao = case_when(
    Regiao == "11" ~ "Rondônia", Regiao == "12" ~ "Acre",
    Regiao == "13" ~ "Amazonas", Regiao == "14" ~ "Roraima",
    Regiao == "15" ~ "Pará", Regiao == "16" ~ "Amapá",
    Regiao == "17" ~ "Tocantins", Regiao == "21" ~ "Maranhão",
    Regiao == "22" ~ "Piauí", Regiao == "23" ~ "Ceará",
    Regiao == "24" ~ "Rio Grande do Norte", Regiao == "25" ~ "Paraíba",
    Regiao == "26" ~ "Pernambuco", Regiao == "27" ~ "Alagoas",
    Regiao == "28" ~ "Sergipe", Regiao == "29" ~ "Bahia",
    Regiao == "31" ~ "Minas Gerais", Regiao == "32" ~ "Espírito Santo",
    Regiao == "33" ~ "Rio de Janeiro", Regiao == "35" ~ "São Paulo",
    Regiao == "41" ~ "Paraná", Regiao == "42" ~ "Santa Catarina",
    Regiao == "43" ~ "Rio Grande do Sul", Regiao == "50" ~ "Mato Grosso do Sul",
    Regiao == "51" ~ "Mato Grosso", Regiao == "52" ~ "Goiás",
    Regiao == "53" ~ "Distrito Federal", Regiao == "Brasil" ~ "Brasil",
    TRUE ~ Regiao  # Mantém os valores que não forem correspondidos
  ))

# Visualizando os dados
head(pnadc_2023_completo)


write.table(pnadc_2023_completo, paste(intdir,"tabela_seguranca_alimentar_pnadc_2023_26marco2025.csv", sep = ""),row.names = F, sep = ";")


