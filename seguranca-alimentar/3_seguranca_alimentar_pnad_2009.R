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
inputdir <- paste(path, "data/inputs/", sep = "/")


dicio_dom <-
  readxl::read_excel(paste0(inputdir,"dicio_pnad_dom_2009.xlsx"),sheet = 'diciopnad2009', range = "A1:E98")
colnames(dicio_dom)

# Criar coluna de fim de posição
dicio_dom <- dicio_dom %>%
  mutate(end = posicao_inicial + tamanho - 1)

colnames(dicio_dom)
## Converte o microdado para um arquivo csv

# Criar especificação para leitura dos microdados
col_specs <- fwf_positions(start = dicio_dom$posicao_inicial, 
                           end = dicio_dom$end, 
                           col_names = dicio_dom$codigo)

pnad_2009 <- read_fwf(paste0(pathdir,'PNAD/2009/Dados/DOM2009.txt'), 
                      col_positions = col_specs, 
                      col_types = cols(.default = "c"))

confere <- pnad_2009 %>% 
  select(UF, V4107) %>% 
  distinct()

pnad_2009 <- pnad_2009 %>%
  mutate(
    V0105 = as.numeric(V0105),   # Total de moradores
    V4623A = as.numeric(V4623A),  # Segurança alimentar
    V4611 = as.numeric(V4611),    # Peso do domicílio
    V4602 = as.numeric(V4602),    # Estrato
    V0102 = as.numeric(V0102)     # UPA (conglomerado primário)
  )


# Criar uma nova variável categórica para facilitar a análise
pnad_2009 <- pnad_2009 %>%
  mutate(seguranca_alimentar = case_when(
    V4623A %in% c(1, 6) ~ "Segurança Alimentar",
    V4623A %in% c(2, 7) ~ "Insegurança Alimentar Leve",
    V4623A %in% c(3, 8) ~ "Insegurança Alimentar Moderada",
    V4623A %in% c(4, 9) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

pnad_2009 <- pnad_2009 %>%
  mutate(situacao_domicilio = case_when(
    V4105 %in% c(1, 2, 3) ~ "Urbano",
    V4105 %in% c(4, 5, 6, 7, 8) ~ "Rural"
  ))



sumarizacao_segalimentar <- pnad_2009 %>%
  group_by(seguranca_alimentar,situacao_domicilio) %>% summarise(num_pessoas = sum(V0105*V4611, na.rm=TRUE),
                                 num_pessoas2 = sum(V0105,na.rm=TRUE),
                                 num_domicilios = sum(V4611, na.rm = TRUE),# Soma ponderada dos pesos (número de domicílios)
                                 num_domicilios2 = n())
