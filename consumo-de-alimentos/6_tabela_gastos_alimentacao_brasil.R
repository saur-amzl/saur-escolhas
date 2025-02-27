

## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

## As tabelas geradas neste script pretendem investigar 
##Alteração no consumo de alimentos e Alteração no consumo de alimentos (FVL)	
# Atrabés da tabela de Aquisição alimentar domiciliar per capita anual (Quilogramas) para os anos de 2008 e 2018

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
base_uc_2008 <- read.csv(paste0(pathdir,"tabela_base_uc_pof0708.csv"),sep = ";")

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_uc_2018 <- read.csv(paste0(pathdir,"tabela_base_uc_pof1718.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_prod_aquisicao_classes_v16fev25.xlsx"),sheet = '2008', range = "A1:AC5136")
tradutor_alimentacao_2008 <-tradutor_alimentacao_2008[c(4:6,19:22,27:29)]
colnames(tradutor_alimentacao_2008) <- c("codigo_2008_trad","produto_2008_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_prod_aquisicao_classes_v16fev25.xlsx"),sheet = '2018', range = "A1:AF4911")
tradutor_alimentacao_2018 <-tradutor_alimentacao_2018[c(2:4,22:25,30:32)]
colnames(tradutor_alimentacao_2018) <- c("codigo_2018_trad","produto_2018_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")


#-- 2008
#Junção dos dados
tb_aux_2008 <- 
  merge( base_aquisicao_alimentar_2008 ,
         tradutor_alimentacao_2008 ,
         by.x = "V9001" ,
         by.y = "codigo_2008_trad")

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$valor_mensal), ] # [2]

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2008 %>%
  group_by(class_final) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_final) %>%
  mutate(indicador_nivel = 'classes_nova')

soma_nivel_2 <- tb_aux_2008 %>%
  group_by(class_analisegeral_final) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final) %>%
  mutate(indicador_nivel = 'alimentos_decreto')

soma_nivel_3 <- tb_aux_2008 %>%
  group_by(item_regional) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = item_regional) %>%
  mutate(indicador_nivel = 'alimentos_regionais')

soma_nivel_4 <- tb_aux_2008 %>%
  group_by(class_analisegeral_final_bebidas) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final_bebidas) %>%
  mutate(indicador_nivel = 'alimentos_decreto_bebidas')

# [1] Empilhando as somas obtidas no passo anterior 
#soma_nivel_1$nivel <- as.character(soma_nivel_1$nivel)
soma_niveis_2008 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4)
soma_uc_2008 <- base_uc_2008 %>% 
  summarise(soma_uc_2008 = sum(PESO_FINAL,na.rm = TRUE))

tab_aquisicao_2008 <- data.frame( soma_niveis_2008 , soma_uc_2008=soma_uc_2008 )

#Verifica a estrutura
str(tab_aquisicao_2008)

tab_aquisicao_2008 <- tab_aquisicao_2008 %>%
  mutate(
    gasto_mensal_familiar_2008 = round(valor_total / soma_uc_2008, 2)
  )


#-- 2018
#Junção dos dados
tb_aux_2018 <- 
  merge( base_aquisicao_alimentar_2018 ,
         tradutor_alimentacao_2018 ,
         by.x = "V9001" ,
         by.y = "codigo_2018_trad")

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

tb_aux_2018 <- tradutor_alimentacao_2018 %>% full_join(base_aquisicao_alimentar_2018, by = c('codigo_2018_trad' = 'V9001'))

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2018 %>%
  group_by(class_final) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_final) %>%
  mutate(indicador_nivel = 'classes_nova')

soma_nivel_2 <- tb_aux_2018 %>%
  group_by(class_analisegeral_final) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final) %>%
  mutate(indicador_nivel = 'alimentos_decreto')

soma_nivel_3 <- tb_aux_2018 %>%
  group_by(item_regional) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = item_regional) %>%
  mutate(indicador_nivel = 'alimentos_regionais')

soma_nivel_4 <- tb_aux_2018 %>%
  group_by(class_analisegeral_final_bebidas) %>%
  summarise(valor_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final_bebidas) %>%
  mutate(indicador_nivel = 'alimentos_decreto_bebidas')


# [1] Empilhando as somas obtidas no passo anterior 
#soma_nivel_1$nivel <- as.character(soma_nivel_1$nivel)
soma_niveis_2018 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4)

soma_uc_2018 <- base_uc_2018 %>% 
  summarise(soma_uc_2018 = sum(PESO_FINAL,na.rm = TRUE))

tab_aquisicao_2018 <- data.frame( soma_niveis_2018 , soma_uc_2018=soma_uc_2018 )

#Verifica a estrutura
str(tab_aquisicao_2018)
tab_aquisicao_2018 <- tab_aquisicao_2018 %>%
  mutate(
    gasto_mensal_familiar_2018 = round(valor_total / soma_uc_2018, 2)
  )

## Unindo as tabelas 
tab_aquisicao_combinada <- full_join(
  tab_aquisicao_2008 %>% select(descricao, indicador_nivel, gasto_mensal_familiar_2008),
  tab_aquisicao_2018 %>% select(descricao, indicador_nivel, gasto_mensal_familiar_2018),
  by = c("descricao","indicador_nivel")
)
colnames(tab_aquisicao_combinada)

write.table(tab_aquisicao_combinada, paste(pathdir,"tabela_gastos_aquisicao_2008_2018_brasil_27fev2025.csv", sep = ""),row.names = F, sep = ";")

