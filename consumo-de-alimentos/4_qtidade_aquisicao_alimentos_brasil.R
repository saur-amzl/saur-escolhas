
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")

source("consumo-de-alimentos/set_estrato.R")

# Etapa 1: Leitura dos dados ----------------------------------------------
#2008-2009
base_aquisicao_alimentar_2008 <- read.csv(paste0(outdir,"tabela_base_alimentacao_pof0809_10marco2025.csv"),sep = ";")
base_pessoas_2008 <- read.csv(paste0(outdir,"tabela_base_pessoas_pof0809_10marco2025.csv"),sep = ";")

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(outdir,"tabela_base_alimentacao_pof1718_10marco2025.csv"),sep = ";")
base_pessoas_2018 <- read.csv(paste0(outdir,"tabela_base_pessoas_pof1718_10marco2025.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(dicdir,"mapeamento_produtos_aquisicao_v16fevereiro25.xlsx"),sheet = '2008', range = "A1:AC5136")
tradutor_alimentacao_2008 <-tradutor_alimentacao_2008[c(4:6,19:22,27:29)]
colnames(tradutor_alimentacao_2008) <- c("codigo_2008_trad","produto_2008_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(dicdir,"mapeamento_produtos_aquisicao_v16fevereiro25.xlsx"),sheet = '2018', range = "A1:AF4911")
tradutor_alimentacao_2018 <-tradutor_alimentacao_2018[c(2:4,22:25,30:32)]
colnames(tradutor_alimentacao_2018) <- c("codigo_2018_trad","produto_2018_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")


#-- 2008
#Junção dos dados
tb_aux_2008 <- 
  merge( base_aquisicao_alimentar_2008 ,
         tradutor_alimentacao_2008 ,
         by.x = "V9001" ,
         by.y = "codigo_2008_trad")

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$qtidade_anual), ] # [2]

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2008 %>%
  group_by(class_final) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_final) %>%
  mutate(indicador_nivel = 'classes_nova')

soma_nivel_2 <- tb_aux_2008 %>%
  group_by(class_analisegeral_final) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final) %>%
  mutate(indicador_nivel = 'alimentos_decreto')

soma_nivel_3 <- tb_aux_2008 %>%
  group_by(item_regional) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = item_regional) %>%
  mutate(indicador_nivel = 'alimentos_regionais')

soma_nivel_4 <- tb_aux_2008 %>%
  group_by(class_analisegeral_final_bebidas) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final_bebidas) %>%
  mutate(indicador_nivel = 'alimentos_decreto_bebidas')

# [1] Empilhando as somas obtidas no passo anterior 
#soma_nivel_1$nivel <- as.character(soma_nivel_1$nivel)
soma_niveis_2008 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4)
soma_individuo_2008 <- base_pessoas_2008 %>% 
  summarise(soma_individuo_2008 = sum(PESO_FINAL,na.rm = TRUE))


tab_aquisicao_2008 <- data.frame( soma_niveis_2008 , soma_individuo_2008=soma_individuo_2008 )

#Verifica a estrutura
str(tab_aquisicao_2008)

tab_aquisicao_2008 <- tab_aquisicao_2008 %>%
  mutate(
    qtd_anual_percapita_2008 = round(qtidade_total / soma_individuo_2008, 2)
  )

#-- 2018
#Junção dos dados
tb_aux_2018 <- 
  merge( base_aquisicao_alimentar_2018 ,
         tradutor_alimentacao_2018 ,
         by.x = "V9001" ,
         by.y = "codigo_2018_trad")

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$qtidade_anual), ] # [2]

tb_aux_2018 <- tradutor_alimentacao_2018 %>% full_join(base_aquisicao_alimentar_2018, by = c('codigo_2018_trad' = 'V9001'))

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2018 %>%
  group_by(class_final) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_final) %>%
  mutate(indicador_nivel = 'classes_nova')

soma_nivel_2 <- tb_aux_2018 %>%
  group_by(class_analisegeral_final) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final) %>%
  mutate(indicador_nivel = 'alimentos_decreto')

soma_nivel_3 <- tb_aux_2018 %>%
  group_by(item_regional) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = item_regional) %>%
  mutate(indicador_nivel = 'alimentos_regionais')

soma_nivel_4 <- tb_aux_2018 %>%
  group_by(class_analisegeral_final_bebidas) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE)) %>%
  rename(descricao = class_analisegeral_final_bebidas) %>%
  mutate(indicador_nivel = 'alimentos_decreto_bebidas')


# [1] Empilhando as somas obtidas no passo anterior 
#soma_nivel_1$nivel <- as.character(soma_nivel_1$nivel)
soma_niveis_2018 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3,soma_nivel_4)
soma_individuo_2018 <- base_pessoas_2018 %>% 
  summarise(soma_individuo_2018 = sum(PESO_FINAL,na.rm = TRUE))


tab_aquisicao_2018 <- data.frame( soma_niveis_2018 , soma_individuo_2018=soma_individuo_2018 )

#Verifica a estrutura
str(tab_aquisicao_2018)
tab_aquisicao_2018 <- tab_aquisicao_2018 %>%
  mutate(
    qtd_anual_percapita_2018 = round(qtidade_total / soma_individuo_2018, 2)
  )

## Unindo as tabelas 
tab_aquisicao_combinada <- full_join(
  tab_aquisicao_2008 %>% select(descricao, indicador_nivel, qtd_anual_percapita_2008),
  tab_aquisicao_2018 %>% select(descricao, indicador_nivel, qtd_anual_percapita_2018),
  by = c("descricao","indicador_nivel")
)

write.table(tab_aquisicao_combinada, paste(outdir,"tab_aquisicao_qtidade_percapita_classes_2008_2018_brasil_10marco2025.csv", sep = ""),row.names = F, sep = ";")


