

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
tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"2007-2008/Tradutores_das_Tabelas_20231009/POF_2008-2009_Codigos_de_alimentacao.xls"),sheet = 'Componentes', range = "A3:G2342")
colnames(tradutor_alimentacao_2008) <- c("Codigo","Nivel_1_08","Descricao_1_08","Nivel_2_08","Descricao_2_08","Nivel_3_08","Descricao_3_08")

tradutor_aquisicao_alimentar_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Aquisi‡ֶo_Alimentar.xls"))
colnames(tradutor_aquisicao_alimentar_2018)

tradutor_geral <- merge(tradutor_alimentacao_2008,
                        tradutor_aquisicao_alimentar_2018,
                        by.x = "Codigo",
                        by.y = "Codigo")


#-- 2008
#Junção dos dados
tb_aux_2008 <- 
  merge( base_aquisicao_alimentar_2008_amzl ,
         tradutor_aquisicao_alimentar_2018 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$qtidade_anual), ] # [2]


#--------
# Instalar e carregar o pacote survey
install.packages("survey")
library(survey)
library(dplyr)

options( survey.lonely.psu = "adjust" )

estrato_dados <- tb_aux_2018 

# Criar o desenho amostral
design <- svydesign(
  ids = ~COD_UPA,               # Conglomerados primários
  strata = ~ESTRATO_POF,  # Estratos
  weights = ~PESO_FINAL,        # Pesos amostrais
  data = estrato_dados
)

# Estimar o total de renda
total_quantidade_aquisicao <- svytotal(~qtidade_anual, design)
survey_mean(~qtidade_anual, design)
# Variância estimada
variancia_aquisicao_qtidade <- attr(total_quantidade_aquisicao, "var")

# Erro padrão
erro_padrao <- sqrt(variancia_aquisicao_qtidade)

# Coeficiente de variação (CV)
cv_aquisicao_ <- (erro_padrao / coef(total_quantidade_aquisicao)) * 100

# Exibir resultados formatados
cat("Estrato analisado:", "TO-UF", "\n")
cat("Total estimado de quantidade anual:", format(coef(total_quantidade_aquisicao), scientific = TRUE), "\n")
cat("Erro padrão:", format(erro_padrao, scientific = TRUE), "\n")
cat("Coeficiente de variação (CV):", round(cv_aquisicao_, 2), "%\n")

#--------------------------

# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2008 %>%
  group_by(Nivel_1,Descricao_1,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_nivel_2 <- tb_aux_2008 %>%
  group_by(Nivel_2,Descricao_2,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_2, descricao = Descricao_2) %>%
  mutate(indicador_nivel = 2)

soma_nivel_3 <- tb_aux_2008 %>%
  group_by(Nivel_3,Descricao_3,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_3, descricao = Descricao_3) %>%
  mutate(indicador_nivel = 3)

# [1] Empilhando as somas obtidas no passo anterior 
#soma_nivel_1$nivel <- as.character(soma_nivel_1$nivel)
soma_niveis_2008 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3)
soma_individuo_2008 <- base_pessoas_2008_amzl %>% 
  group_by(descricao_estrato)  %>% 
  summarise(soma_individuo_2008 = sum(PESO_FINAL,na.rm = TRUE))


tab_aquisicao_2008 <- merge(soma_niveis_2008, soma_individuo_2008, by = "descricao_estrato", all.x = TRUE)

#Verifica a estrutura
str(tab_aquisicao_2008)

tab_aquisicao_2008 <- tab_aquisicao_2008 %>%
  mutate(
    qtd_anual_percapita_2008 = round(qtidade_total / soma_individuo_2008, 2)
  )

#-- 2018
#Junção dos dados
tb_aux_2018 <- 
  merge( base_aquisicao_alimentar_2018_amzl,
         tradutor_aquisicao_alimentar_2018 ,
         by.x = "codigo" ,
         by.y = "Codigo")

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$qtidade_anual), ] # [2]
# Somando os valores mensais de cada grupo de codigos, segundo cada nivel, conforme consta no tradutor
soma_nivel_1 <- tb_aux_2018 %>%
  group_by(Nivel_1,Descricao_1,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_1, descricao = Descricao_1) %>%
  mutate(indicador_nivel = 1)

soma_nivel_2 <- tb_aux_2018 %>%
  group_by(Nivel_2,Descricao_2,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_2, descricao = Descricao_2) %>%
  mutate(indicador_nivel = 2)

soma_nivel_3 <- tb_aux_2018 %>%
  group_by(Nivel_3,Descricao_3,descricao_estrato) %>%
  summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE),
            valor_mensal_total = sum(valor_mensal, na.rm = TRUE)) %>%
  rename(nivel = Nivel_3, descricao = Descricao_3) %>%
  mutate(indicador_nivel = 3)

# [1] Empilhando as somas obtidas no passo anterior 
soma_niveis_2018 <- rbind(soma_nivel_1,soma_nivel_2 ,soma_nivel_3) # [1]
rm(soma_nivel_1,soma_nivel_2 ,soma_nivel_3)
soma_individuo_2018 <- base_pessoas_2018_amzl %>% 
         group_by(descricao_estrato)  %>% 
         summarise(soma_individuo_2018 = sum(PESO_FINAL,na.rm = TRUE))


tab_aquisicao_2018 <- merge(soma_niveis_2018, soma_individuo_2018, by = "descricao_estrato", all.x = TRUE)

#Verifica a estrutura
str(tab_aquisicao_2018)

tab_aquisicao_2018 <- tab_aquisicao_2018 %>%
  mutate(
    qtd_anual_percapita_2018 = round(qtidade_total / soma_individuo_2018, 2)
  )

## Unindo as tabelas 
tab_aquisicao_combinada <- full_join(
  tab_aquisicao_2008 %>% select(descricao_estrato, nivel,descricao, indicador_nivel, qtd_anual_percapita_2008),
  tab_aquisicao_2018 %>% select(descricao_estrato, nivel,descricao, indicador_nivel, qtd_anual_percapita_2018),
  by = c("descricao_estrato","nivel","descricao","indicador_nivel")
)
colnames(tab_aquisicao_combinada)

write.table(tab_aquisicao_combinada, paste(pathdir,"tabela_aquisicao_2008_2018_amzlegal_3dec2024.csv", sep = ""),row.names = F, sep = ";")

# Lista de código de    Frutas de clima temperado,    Frutas de clima tropical,    Hortaliças folhosas e florais,   Hortaliças frutosas
#    Hortaliças tuberosas e outras,    Leguminosas
cod_flv <- c(12101, 12102, 11201, 11202, 11101, 12103, 11102, 11103, 11301, 
                   12201, 11203, 12104, 12105, 12106, 12107, 12108, 11306, 11302, 
                   11303, 11304, 11305, 11204, 11307, 12202, 11308, 11205, 11309, 
                   11104, 11206, 11105, 11106, 11107, 10201, 10202, 10203, 10204, 
                   10205, 10206, 10207, 12110, 11310, 11207, 12111, 12112, 12113, 
                   12114, 12116, 12203, 12117, 11311, 12118, 12119, 11208, 12120, 
                   12121, 12204, 10209, 11109, 11214, 11312, 12123, 12208, 12109, 
                   12115, 10208, 11209, 12205, 12206, 11210, 11211, 11108, 12122, 
                   11212, 12207, 11213)


tab_aquisicao_combinada_flv <- tab_aquisicao_combinada[tab_aquisicao_combinada$nivel %in% cod_flv, ]

write.table(tab_aquisicao_combinada_flv, paste(pathdir,"tabela_aquisicao_flv_2008_2018_amzlegal_4dec2024.csv", sep = ""),row.names = F, sep = ";")


