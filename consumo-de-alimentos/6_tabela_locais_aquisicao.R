

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
#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_uc_2018 <- read.csv(paste0(pathdir,"tabela_base_uc_pof1718.csv"),sep = ";")


# Selecionando apenas estratos de interesse: AMZLEGAL
#2017-2018
base_aquisicao_alimentar_2018_amzl <- base_aquisicao_alimentar_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

base_uc_2018_amzl <- base_uc_2018 %>%
  right_join(tabela_estratos_2018, by = c("UF", "ESTRATO_POF"))

rm(base_aquisicao_alimentar_2018,base_uc_2018)

#Mapeamento GUIA(NOVA)
mapeamento_nova_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_alimentacaopof1718_nova_br.xlsx"),sheet = 'Sheet1', range= "A1:Q8801")
colnames(mapeamento_nova_alimentacao_2018)

tradutor_locais_2018_amzl <-
  readxl::read_excel(paste0(pathdir,"mapeamento_local_pof1718_amzl.xlsx"),sheet = 'Sheet1', range= "A1:E318")

#-- 2018
tb_aux_2018 <- base_aquisicao_alimentar_2018_amzl %>%
  left_join(mapeamento_nova_alimentacao_2018, by=c("V9001"="codigo_2018"))

tb_aux_2018 <-  tb_aux_2018 %>%
  left_join(tradutor_locais_2018_amzl, by=c("V9004"="codigo_local"))

tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

#Onde nao tem descricao do local é pq não foi informado
tb_aux_2018 %>% filter(is.na(nome_local))

# Tabela que inclui variável de contagem 
#2018
tabela_aquisicao_2018 <- tb_aux_2018 %>%
  mutate(count = 1) %>%  # Criando variável de contagem
  group_by(descricao_estrato, UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC,classe_prod,nome_local,cod_local) %>%
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
  group_by(descricao_estrato.x,classe_prod,nome_local,cod_local) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))
colnames(total_alimentos_nova_2018_local)


tabela6 <- total_alimentos_nova_2018_local %>%
  mutate(classe_prod = recode(classe_prod,
                              "In natura ou minimamente processado" = "natura",
                              "Ultraprocessado" = "ultraprocessado",
                              "Processado" = "processado",
                              "preparacao_culinaria" = "prep_culinaria",
                              "oleos_gorduras_sal_acucar" = "ingredientes_culinarios",
                              "Bebida Alcoolica" = "bebida_alcoolica",
                              "sem_classificacao" = "sem_classe"))  %>%
    pivot_wider(
      names_from = classe_prod, # Nome das colunas a partir de classe_prod
      values_from = total_2018, # Valores preenchidos por total_2018
      values_fill = list(total_2018 = 0) # Preencher valores ausentes com 0
  ) %>%
  # Calcular a soma total por linha (opcional)
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)))) %>%
  ungroup()  %>%  
  # Calcular os percentuais para cada classe_prod
  mutate(across(
    .cols = bebida_alcoolica:sem_classe, # Colunas de produtos
    .fns = ~ (.x / total) * 100, # Fórmula para calcular percentual
    .names = "perc_{.col}" # Nome da nova coluna de percentual
  )) %>%
  # Ordenar a tabela pelo cod_local
  arrange(descricao_estrato.x, cod_local)


tabela7 <- total_alimentos_nova_2018_local %>%
  mutate(classe_prod = recode(classe_prod,
                              "In natura ou minimamente processado" = "natura",
                              "Ultraprocessado" = "ultraprocessado",
                              "Processado" = "processado",
                              "preparacao_culinaria" = "prep_culinaria",
                              "oleos_gorduras_sal_acucar" = "ingredientes_culinarios",
                              "Bebida Alcoolica" = "bebida_alcoolica",
                              "sem_classificacao" = "sem_classe"))  %>%
  pivot_wider(
    names_from = classe_prod, # Nome das colunas a partir de classe_prod
    values_from = total_2018, # Valores preenchidos por total_2018
    values_fill = list(total_2018 = 0) # Preencher valores ausentes com 0
  ) %>%
  # Calcular o total para cada nome_local
  group_by(descricao_estrato.x, nome_local, cod_local) %>%
  mutate(total_local = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  # Calcular o total de cada produto dentro de cada descricao_estrato.x
  group_by(descricao_estrato.x) %>%
  mutate(across(
    bebida_alcoolica:sem_classe, # Colunas de produtos
    .fns = ~ .x / sum(.x, na.rm = TRUE), # Percentual em relação ao total por descricao_estrato
    .names = "perc_{.col}" # Nome das novas colunas de percentual
  )) %>%
  ungroup() %>%
  # Ordenar pelo cod_local (opcional)
  arrange(descricao_estrato.x, cod_local)



  
write.table(tabela6, paste(pathdir,"tabela_total_alimentos_nova_local_2018_amzlegal.csv", sep = ""),row.names = F, sep = ";")
write.table(tabela7, paste(pathdir,"tabela_total_alimentos_nova_local_percprod_2018_amzlegal.csv", sep = ""),row.names = F, sep = ";")

write.table(total_alimentos_nova_local_combinada, paste(pathdir,"tabela_total_alimentos_nova_local_2008_2018_amzlegal.csv", sep = ""),row.names = F, sep = ";")
