
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

caderneta_coletiva <- readRDS(paste0(pathdir,"pof_fam_wide_2018.rds"))


# Etapa 1: Leitura dos dados ----------------------------------------------
pos_estrato <-
  readxl::read_excel(
    paste0(pathdir,"2017_2018/Documentacao_20230713/Pos_estratos_totais.xlsx"),skip=5) # [1]


#2007-2008
base_uc_2008 <- read.csv(paste0(pathdir,"tabela_base_uc_pof0708.csv"),sep = ";")
base_pessoas_2008 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof0708.csv"),sep = ";")

#2017-2018
base_uc_2018 <- read.csv(paste0(pathdir,"tabela_base_uc_pof1718.csv"),sep = ";")
base_pessoas_2018 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof1718.csv"),sep = ";")


# Número de familias (ucs)
sum(base_uc_2008$NUM_UC)
sum(base_uc_2008$PESO_FINAL)
sum(base_uc_2018$NUM_UC)
sum(base_uc_2018$PESO_FINAL)


## indivíduos
sum(base_pessoas_2008$NUM_UC)
sum(base_pessoas_2008$PESO_FINAL)
sum(base_pessoas_2018$NUM_UC)
sum(base_pessoas_2018$PESO_FINAL)



# Selecionando apenas estratos de interesse: AMZLEGAL
base_uc_2008_amzl <- base_uc_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_pessoas_2008_amzl <- base_pessoas_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_uc_2018_amzl <- base_uc_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

base_pessoas_2018_amzl <- base_pessoas_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))



soma_familia <- sum(base_uc_2008$PESO_FINAL);soma_familia
soma_ind <- sum(base_pessoas_2008$PESO_FINAL);soma_ind
soma_ind/soma_familia


soma_num_uc <- sum(base_uc_2008$NUM_UC);soma_num_uc
soma_num_uc <- sum(base_uc_2008$NUM_DOM);soma_num_uc
soma_ind_ss <- sum(base_pessoas_2008$NUM_UC);soma_ind_ss


soma_ind_ss/soma_num_uc


soma_familia <- sum(base_uc_2018$PESO_FINAL);soma_familia
soma_ind <- sum(base_pessoas_2018$PESO_FINAL);soma_ind
soma_ind/soma_familia


familias_2008_est <- base_uc_2008_amzl %>%
  group_by(descricao_estrato) %>%
  summarise(total_familia_2008 = sum(PESO_FINAL,na.rm=TRUE),
            total_familia_2008_numuc = sum(NUM_UC,na.rm=TRUE))

familias_2018_est <- base_uc_2018_amzl %>%
  group_by(descricao_estrato) %>%
  summarise(total_familia_2018 = sum(PESO_FINAL,na.rm=TRUE),
            total_familia_2018_numuc = sum(NUM_UC,na.rm=TRUE))


ind_2008_est <- base_pessoas_2008_amzl %>%
  group_by(descricao_estrato) %>%
  summarise(total_ind_2008 = sum(PESO_FINAL,na.rm=TRUE),
            total_ind_2008_numuc = sum(NUM_UC,na.rm=TRUE),)

ind_2018_est <- base_pessoas_2018_amzl %>%
  group_by(descricao_estrato) %>%
  summarise(total_ind_2018 = sum(PESO_FINAL,na.rm=TRUE),
            total_ind_2018_numuc = sum(NUM_UC,na.rm=TRUE))



# Unindo as tabelas
tabela_unificada <- familias_2008_est %>%
  full_join(familias_2018_est, by = "descricao_estrato") %>%
  full_join(ind_2008_est, by = "descricao_estrato") %>%
  full_join(ind_2018_est, by = "descricao_estrato")


write.table(tabela_unificada, paste(pathdir,"tabela_pessoas_familias_2008_2018_amzlegal_13jan2025.csv", sep = ""),row.names = F, sep = ";")

