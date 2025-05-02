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
pathdir <- paste(path, "data/raw/", sep = "/")
intdir <-  paste(path, "data/intermediate/", sep = "/")


# Etapa 1: Leitura dos dados ----------------------------------------------------
dom_pof_2018 <- readRDS(paste0(pathdir,"2017-2018/Dados_20230713/DOMICILIO.rds"))
morador_pof_2018 <- readRDS(paste0(pathdir,"2017-2018/Dados_20230713/MORADOR.rds"))

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

morador_2018 <- morador_pof_2018 %>% 
  select(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,NUM_UC,COD_INFORMANTE,PESO_FINAL) %>% 
  left_join(dom_pof_2018 %>% select(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,V6199),
            c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM"))
  

morador_2018 <- morador_2018 %>%
  mutate(seguranca_alimentar = case_when(
    V6199 %in% c(1) ~ "Segurança Alimentar",
    V6199 %in% c(2) ~ "Insegurança Alimentar Leve",
    V6199 %in% c(3) ~ "Insegurança Alimentar Moderada",
    V6199 %in% c(4) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))


morador_2018 <- morador_2018 %>%
  mutate(situacao_domicilio = case_when(
    TIPO_SITUACAO_REG %in% c(1) ~ "Urbano",
    TIPO_SITUACAO_REG %in% c(2) ~ "Rural"
  ))

#sumarizacao_segalimentar <- pof_2018 %>%
#  group_by(seguranca_alimentar,situacao_domicilio) %>% 
#  summarise(num_pessoas = sum(PESO_FINAL, na.rm=TRUE))

# Gerando o dado para o Brasil
pof_2018_brasil_domicilio <- dom_pof_2018 %>%
  mutate(Regiao = "Brasil") %>%
  group_by(Regiao, seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_domicilios = sum(PESO_FINAL, na.rm = TRUE)) 

pof_2018_brasil_morador <- morador_2018 %>%
  mutate(Regiao = "Brasil") %>%
  group_by(Regiao, seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(PESO_FINAL, na.rm = TRUE)) 


# Gerando o dado para os estados
pof_2018_estados_domicilio <- dom_pof_2018 %>%
  group_by(UF,seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_domicilios = sum(PESO_FINAL, na.rm = TRUE)) %>% 
  mutate(UF = as.character(UF)) %>%  # Conversão aqui
  rename(Regiao = UF)

pof_2018_estados_morador <- morador_2018 %>%
  group_by(UF,seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(PESO_FINAL, na.rm = TRUE)) %>% 
  mutate(UF = as.character(UF)) %>%  # Conversão aqui
  rename(Regiao = UF)

# Juntando os dois dataframes
pof_2018_domicilio <- bind_rows(pof_2018_brasil_domicilio, pof_2018_estados_domicilio)
pof_2018_morador <- bind_rows(pof_2018_brasil_morador, pof_2018_estados_morador)


pof_2018_completo <- full_join(pof_2018_domicilio, pof_2018_morador, 
                               by = c("Regiao", "seguranca_alimentar", "situacao_domicilio"))


# Substituindo os códigos pelos nomes dos estados usando case_when
pof_2018_completo <- pof_2018_completo %>%
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
head(pof_2018_completo)


write.table(pof_2018_completo, paste(intdir,"tabela_seguranca_alimentar_pof_2018_26marco2025.csv", sep = ""),row.names = F, sep = ";")

