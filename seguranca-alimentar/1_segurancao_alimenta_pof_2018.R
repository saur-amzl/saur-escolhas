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

source("consumo-de-alimentos/set_estrato.R")

#base_domicilio_2018 <- read.csv(paste0(pathdir,"tabela_base_uc_pof1718.csv"),sep = ";")
#base_pessoas_2018 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof1718.csv"),sep = ";")


dom_pof_2018 <- readRDS(paste0(pathdir,"2017_2018/Dados_20230713/DOMICILIO.rds"))


V6199


dom_pof_2018 <- dom_pof_2018 %>%
  mutate(seguranca_alimentar = case_when(
    V6199 %in% c(1) ~ "Segurança Alimentar",
    V6199 %in% c(2) ~ "Insegurança Alimentar Leve",
    V6199 %in% c(3) ~ "Insegurança Alimentar Moderada",
    V6199 %in% c(4) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

dom_pof_2018 <- dom_pof_2018 %>%
  mutate(situacao_domicilio = case_when(
    TIPO_SITUACAO_REG %in% c(1) ~ "Urbano",
    TIPO_SITUACAO_REG %in% c(2) ~ "Rural"
  ))

sumarizacao_segalimentar <- dom_pof_2018 %>%
  group_by(seguranca_alimentar,TIPO_SITUACAO_REG) %>% summarise(num_domicilios = sum(PESO_FINAL, na.rm=TRUE))




morador <- readRDS(paste0(pathdir,"2017_2018/Dados_20230713/MORADOR.rds"))
##########################################################################

# Desenho para a estimativa dos moradores segundo situa??o de 
# seguran?a alimentar
colnames(morador)
#morador_novo <- morador %>% 
#  filter(NUM_UC == 1) %>% 
#  group_by(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,NUM_UC,COD_INFORMANTE) %>% 
#  summarise(num_moradores = n()) %>% 

#filter(NUM_UC == 1) %>%
teste <- morador %>% 
  select(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,NUM_UC,COD_INFORMANTE,PESO_FINAL) %>% 
  left_join(dom_pof_2018 %>% select(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,V6199),
            c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM"))
  

teste <- teste %>%
  mutate(seguranca_alimentar = case_when(
    V6199 %in% c(1) ~ "Segurança Alimentar",
    V6199 %in% c(2) ~ "Insegurança Alimentar Leve",
    V6199 %in% c(3) ~ "Insegurança Alimentar Moderada",
    V6199 %in% c(4) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))


teste <- teste %>%
  mutate(situacao_domicilio = case_when(
    TIPO_SITUACAO_REG %in% c(1) ~ "Urbano",
    TIPO_SITUACAO_REG %in% c(2) ~ "Rural"
  ))

sumarizacao_segalimentar <- teste %>%
  group_by(seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(PESO_FINAL, na.rm=TRUE))


