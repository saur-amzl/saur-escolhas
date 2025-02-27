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
outdir <- paste(path, "data/outputs/", sep = "/")

# Etapa 1: Leitura dos dados ----------------------------------------------------
dicio_dom <-
  readxl::read_excel(paste0(inputdir,"dicio_pnad_dom_2004.xlsx"),sheet = 'diciopnad2004', range = "A1:E120")
colnames(dicio_dom)

# Criar coluna de fim de posição
dicio_dom <- dicio_dom %>%
  mutate(end = posicao_inicial + tamanho - 1)

# Criar especificação para leitura dos microdados
col_specs <- fwf_positions(start = dicio_dom$posicao_inicial, 
                           end = dicio_dom$end, 
                           col_names = dicio_dom$codigo)

pnad_2004 <- read_fwf(paste0(pathdir,"PNAD/2004/Dados/DOM2004.txt"), 
                      col_positions = col_specs, 
                      col_types = cols(.default = "c"))

#Etapa 3: Processamento -------------------------------------------------------
pnad_2004 <- pnad_2004 %>%
  mutate(
    V0105 = as.numeric(V0105),   # Total de moradores
    V4623A = as.numeric(V4623A),  # Segurança alimentar
    V4611 = as.numeric(V4611),    # Peso do domicílio
    V4602 = as.numeric(V4602)    # Estrato
  )


# Criar uma nova variável categórica para facilitar a análise
pnad_2004 <- pnad_2004 %>%
  mutate(seguranca_alimentar = case_when(
    V4623A %in% c(1, 6) ~ "Segurança Alimentar",
    V4623A %in% c(2, 7) ~ "Insegurança Alimentar Leve",
    V4623A %in% c(3, 8) ~ "Insegurança Alimentar Moderada",
    V4623A %in% c(4, 9) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

pnad_2004 <- pnad_2004 %>%
  mutate(situacao_domicilio = case_when(
    V4105 %in% c(1, 2, 3) ~ "Urbano",
    V4105 %in% c(4, 5, 6, 7, 8) ~ "Rural"
  ))



# Gerando o dado para o Brasil
pnad_2004_brasil <- pnad_2004 %>%
  mutate(Regiao = "Brasil") %>%
  group_by(Regiao, seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V0105*V4611, na.rm=TRUE),
            num_domicilios = sum(V4611, na.rm = TRUE)) 


# Gerando o dado para os estados
pnad_2004_estados <- pnad_2004 %>%
  group_by(UF,seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V0105*V4611, na.rm=TRUE),
            num_domicilios = sum(V4611, na.rm = TRUE)) %>% 
  rename(Regiao = UF)

# Juntando os dois dataframes
pnad_2004_completo <- bind_rows(pnad_2004_brasil, pnad_2004_estados)

# Substituindo os códigos pelos nomes dos estados usando case_when
pnad_2004_completo <- pnad_2004_completo %>%
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
head(pnad_2004_completo)


write.table(pnad_2004_completo, paste(outdir,"tabela_seguranca_alimentar_pnad_2004_26marco2025.csv", sep = ""),row.names = F, sep = ";")
