

## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

## As tabelas geradas neste script pretendem investigar 
## Local de compra de alimentos
# Através da tabela Percentual de aquisição de alimentos, segundo categorias do Guia Alimentar, por tipo de estabelecimento para os anos de 2008 e 2018

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


# Selecionando apenas estratos de interesse: AMZLEGAL
#2007-2008
base_aquisicao_alimentar_2008_amzl <- base_aquisicao_alimentar_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_uc_2008_amzl <- base_uc_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

#2017-2018
base_aquisicao_alimentar_2018_amzl <- base_aquisicao_alimentar_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

base_uc_2018_amzl <- base_uc_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

rm(base_aquisicao_alimentar_2018,base_uc_2018,base_uc_2008,base_aquisicao_alimentar_2008)


#Tradutor - Aquisicao de alimentos
#tradutor_aquisicao_alimentar_2018 <-
#  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Aquisi‡ֶo_Alimentar.xls"))

mapeamento_nova_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_alimentacaopof0708_nova_br.xlsx"),sheet = 'Sheet1', range= "A1:M3211")
colnames(mapeamento_nova_alimentacao_2008)

mapeamento_nova_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_alimentacaopof1718_nova_br.xlsx"),sheet = 'Sheet1', range= "A1:Q8801")
colnames(mapeamento_nova_alimentacao_2018)

tradutor_locais_2018 <- read.csv(paste0(pathdir,"tradutor_locais_aqui_pof1718.csv"),sep = ";")
colnames(tradutor_locais_2018)

tradutor_locais_2018v2 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_local_pof.xlsx"),sheet = 'Sheet1', range= "A1:F795")


#-- 2008
tb_aux_2008 <- base_aquisicao_alimentar_2008_amzl %>%
  left_join(mapeamento_nova_alimentacao_2008, by=("V9001"="V9001"))

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$valor_mensal), ] # [2]

tb_aux_2008 <-  tb_aux_2008 %>%
  left_join(tradutor_locais_2018, by=c("V9004"="codigo_local"))

tb_aux_2008 <- tb_aux_2008[!is.na(tb_aux_2008$valor_mensal), ] # [2]

#-- 2018
tb_aux_2018 <- base_aquisicao_alimentar_2018_amzl %>%
  left_join(mapeamento_nova_alimentacao_2018, by=c("V9001"="codigo_2018"))

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

tb_aux_2018 <-  tb_aux_2018 %>%
  left_join(tradutor_locais_2018v2, by=c("V9004"="codigo_local"))

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

# Tabela que inclui variável de contagem 
#2008
tabela_aquisicao_2008 <- tb_aux_2008 %>%
  mutate(count = 1) %>%  # Criando variável de contagem
  group_by(descricao_estrato,COD_UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC,classe_prod,nome_local) %>%
  summarize(count = sum(count, na.rm = TRUE),
            valor_mensal = sum(valor_mensal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(key_morador = paste0(descricao_estrato,COD_UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC)) 

base_uc_2008_amzl <- base_uc_2008_amzl %>%
  mutate(across(c(COD_UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC), as.character)) %>%
  mutate(key_morador = paste0(descricao_estrato,COD_UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC))

tabela_final_2008 <- inner_join(tabela_aquisicao_2008, base_uc_2008_amzl, by = "key_morador")

total_alimentos_nova_2008 <- tabela_final_2008 %>%
  mutate(itens = count * PESO_FINAL) %>%
  group_by(descricao_estrato.x,classe_prod) %>%
  summarise(total_2008 = sum(itens, na.rm = TRUE))

total_alimentos_nova_2008_local <- tabela_final_2008 %>%
  mutate(itens = count * PESO_FINAL) %>%
  group_by(descricao_estrato.x,classe_prod,nome_local) %>%
  summarise(total_2008 = sum(itens, na.rm = TRUE))

#2018
tabela_aquisicao_2018 <- tb_aux_2018 %>%
  mutate(count = 1) %>%  # Criando variável de contagem
  group_by(descricao_estrato, UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC,classe_prod,nome_local) %>%
  summarize(count = sum(count, na.rm = TRUE),
            valor_mensal = sum(valor_mensal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(key_morador = paste0(descricao_estrato,UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC)) 

base_uc_2018_amzl <- base_uc_2018_amzl %>%
  mutate(across(c(descricao_estrato,UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC), as.character)) %>%
  mutate(key_morador = paste0(descricao_estrato,UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC))

# Juntando com a base morador_uc_key
tabela_final_2018 <- inner_join(tabela_aquisicao_2018, base_uc_2018_amzl, by = "key_morador")

total_alimentos_nova_2018 <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  group_by(descricao_estrato.x,classe_prod) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))
colnames(total_alimentos_nova_2018)

total_alimentos_nova_2018_local <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  group_by(descricao_estrato.x,classe_prod,nome_local) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))
colnames(total_alimentos_nova_2018)

## Unindo as tabelas 
total_alimentos_nova_combinada <- full_join(
  total_alimentos_nova_2008 %>% select(descricao_estrato.x, classe_prod, total_2008),
  total_alimentos_nova_2018 %>% select(descricao_estrato.x, classe_prod, total_2018),
  by = c("descricao_estrato.x","classe_prod")
)

total_alimentos_nova_local_combinada <- full_join(
  total_alimentos_nova_2008_local %>% select(descricao_estrato.x, classe_prod,nome_local, total_2008),
  total_alimentos_nova_2018_local %>% select(descricao_estrato.x, classe_prod,nome_local, total_2018),
  by = c("descricao_estrato.x","classe_prod","nome_local")
)

write.table(total_alimentos_nova_combinada, paste(pathdir,"tabela_total_alimentos_nova_2008_2018_amzlegal.csv", sep = ""),row.names = F, sep = ";")

write.table(total_alimentos_nova_local_combinada, paste(pathdir,"tabela_total_alimentos_nova_local_2008_2018_amzlegal.csv", sep = ""),row.names = F, sep = ";")
