

## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

## As tabelas geradas neste script pretendem investigar 
## Gastos com alimentação	
# Através da tabela de Despesa monetária e não monetária média mensal familiar com alimentação no domicílio e fora do domicílio, por forma de aquisição para os anos de 2008 e 2018

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")

source("consumo-de-alimentos/set_estrato.R")

# Etapa 1: Leitura dos dados ----------------------------------------------
#2007-2008
base_aquisicao_alimentar_2008 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof0708.csv"),sep = ";")
base_pessoas_2008 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof0708.csv"),sep = ";")

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_pessoas_2018 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof1718.csv"),sep = ";")


# Selecionando apenas estratos de interesse: AMZLEGAL
base_aquisicao_alimentar_2008_amzl <- base_aquisicao_alimentar_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_pessoas_2008_amzl <- base_pessoas_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_aquisicao_alimentar_2018_amzl <- base_aquisicao_alimentar_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

base_pessoas_2018_amzl <- base_pessoas_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

rm(base_aquisicao_alimentar_2008,base_pessoas_2008,base_aquisicao_alimentar_2018,base_pessoas_2018)

#Tradutor - Aquisicao de alimentos
#tradutor_aquisicao_alimentar_2018 <-
#  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Aquisi‡ֶo_Alimentar.xls"))

tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"2007-2008/Tradutores_das_Tabelas_20231009/POF_2008-2009_Codigos_de_alimentacao.xls"),sheet = 'Componentes', range = "A3:G2342")
colnames(tradutor_alimentacao_2008) <- c("Codigo","Nivel_1","Descricao_1","Nivel_2","Descricao_2","Nivel_3","Descricao_3")
tradutor_alimentacao_2018 <- readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Alimenta‡ֶo.xls"))

#-- 2008
#Junção dos dados
tb_aux_2008 <- 
  merge( base_aquisicao_alimentar_2008_amzl ,
         tradutor_alimentacao_2008 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$valor_mensal), ] # [2]

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2008 %>%
  group_by(Nivel_1,Descricao_1,descricao_estrato,V9002) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_niveis_2008 <- soma_nivel_1
rm(soma_nivel_1)
soma_individuo_2008 <- base_pessoas_2008_amzl %>% 
  group_by(descricao_estrato)  %>% 
  summarise(soma_individuo_2008 = sum(PESO_FINAL,na.rm = TRUE))


tab_gastos_2008 <- merge(soma_niveis_2008, soma_individuo_2008, by = "descricao_estrato", all.x = TRUE)

#Verifica a estrutura
str(tab_gastos_2008)

tab_gastos_2008 <- tab_gastos_2008 %>%
  mutate(
    gasto_mensal_percapita_2008 = round(valor_mensal_total / soma_individuo_2008, 2)
  )

#-- 2018
#Junção dos dados
tb_aux_2018 <- 
  merge( base_aquisicao_alimentar_2018_amzl,
         tradutor_alimentacao_2018 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]
# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2018 %>%
  group_by(Nivel_1,Descricao_1,descricao_estrato,V9002) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_niveis_2018 <- soma_nivel_1 
rm(soma_nivel_1)
soma_individuo_2018 <- base_pessoas_2018_amzl %>% 
  group_by(descricao_estrato)  %>% 
  summarise(soma_individuo_2018 = sum(PESO_FINAL,na.rm = TRUE))

tab_gastos_2018 <- merge(soma_niveis_2018, soma_individuo_2018, by = "descricao_estrato", all.x = TRUE)

#Verifica a estrutura
str(tab_gastos_2018)

tab_gastos_2018 <- tab_gastos_2018 %>%
  mutate(
    gasto_mensal_percapita_2018 = round(valor_mensal_total / soma_individuo_2018, 2)
  )

## Unindo as tabelas 
tab_gastos_combinada <- full_join(
  tab_gastos_2008 %>% select(descricao_estrato, nivel, descricao,V9002, indicador_nivel, gasto_mensal_percapita_2008),
  tab_gastos_2018 %>% select(descricao_estrato, nivel, descricao,V9002, indicador_nivel, gasto_mensal_percapita_2018),
  by = c("descricao_estrato","nivel", "descricao","V9002","indicador_nivel")
)


write.table(tab_gastos_combinada, paste(pathdir,"tabela_gastos_forma_aqui_2008_2018_amzlegal_3dec2024.csv", sep = ""),row.names = F, sep = ";")
