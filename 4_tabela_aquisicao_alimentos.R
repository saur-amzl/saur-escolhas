

## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")



# Etapa 1: Leitura dos dados ----------------------------------------------
#2007-2008
base_aquisicao_alimentar_2008 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof0708.csv"),sep = ";")
base_pessoas_2008 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof0708.csv"),sep = ";")

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_pessoas_2018 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof1718.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_aquisicao_alimentar_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Aquisi‡ֶo_Alimentar.xls"))



#-- 2008
#Junção dos dados
tb_aux_2008 <- 
  merge( base_aquisicao_alimentar_2008 ,
         tradutor_aquisicao_alimentar_2018 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$qtidade_anual), ] # [2]

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2008 %>%
  group_by(Nivel_1,Descricao_1) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_nivel_2 <- tb_aux_2008 %>%
  group_by(Nivel_2,Descricao_2) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_2, descricao = Descricao_2) %>%
  mutate(indicador_nivel = 2)

soma_nivel_3 <- tb_aux_2008 %>%
  group_by(Nivel_3,Descricao_3) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_3, descricao = Descricao_3) %>%
  mutate(indicador_nivel = 3)

# [1] Empilhando as somas obtidas no passo anterior 
soma_niveis_2008 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3)
soma_individuo_2008 <- sum(base_pessoas_2008$PESO_FINAL )


tab_aquisicao_2008 <- data.frame(soma_niveis_2008, soma_individuo_2008=soma_individuo_2008 )
tab_aquisicao_2008 <- tab_aquisicao_2008 %>%
  mutate(qtd_anual_percapita_2008 = round( qtidade_total / soma_individuo_2008 , 2 ),
         valor_mensal_percapita2008 = round( valor_mensal_total / soma_individuo_2008 , 2 ))


#-- 2018
#Junção dos dados
tb_aux_2018 <- 
  merge( base_aquisicao_alimentar_2018 ,
         tradutor_aquisicao_alimentar_2018 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$qtidade_anual), ] # [2]

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2018 %>%
  group_by(Nivel_1,Descricao_1) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_nivel_2 <- tb_aux_2018 %>%
  group_by(Nivel_2,Descricao_2) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_2, descricao = Descricao_2) %>%
  mutate(indicador_nivel = 2)

soma_nivel_3 <- tb_aux_2018 %>%
  group_by(Nivel_3,Descricao_3) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_3, descricao = Descricao_3) %>%
  mutate(indicador_nivel = 3)

# [1] Empilhando as somas obtidas no passo anterior 
soma_niveis_2018 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3)
soma_individuo_2018 <- sum(base_pessoas_2018$PESO_FINAL )


tab_aquisicao_2018 <- data.frame(soma_niveis_2018, soma_individuo_2018=soma_individuo_2018 )
tab_aquisicao_2018 <- tab_aquisicao_2008 %>%
  mutate(qtd_anual_percapita_2018 = round( qtidade_total / soma_individuo_2018 , 2 ),
         valor_mensal_percapita_2018 = round( valor_mensal_total / soma_individuo_2018 , 2))

## Unindo as tabelas 
tab_aquisicao_combinada <- full_join(
  tab_aquisicao_2008 %>% select(nivel, descricao, indicador_nivel, qtd_anual_percapita_2008, valor_mensal_percapita2008),
  tab_aquisicao_2018 %>% select(nivel, descricao, indicador_nivel, qtd_anual_percapita_2018, valor_mensal_percapita_2018),
  by = c("nivel", "descricao","indicador_nivel")
)


write.table(tab_aquisicao_combinada, paste(pathdir,"tabela_aquisicao_2008_2018.csv", sep = ""),row.names = F, sep = ";")
