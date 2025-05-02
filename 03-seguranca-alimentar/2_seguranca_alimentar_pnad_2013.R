# ------------------------------------------------------------------------------
# Script para calcular segurando alimentar da PNAD 2013
# ------------------------------------------------------------------------------
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
pathdir <- paste(path, "data/raw/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")
outdir <- paste(path, "data/outputs/", sep = "/")

# Etapa 1: Leitura dos dados ----------------------------------------------------
dicio_dom <-
  readxl::read_excel(paste0(dicdir,"dicio_pnad_dom_2013.xlsx"),sheet = 'diciopnad2013', range = "A1:E100")

# Criar coluna de fim de posição
dicio_dom <- dicio_dom %>% mutate(end = posicao_inicial + tamanho - 1)

# Criar especificação para leitura dos microdados
col_specs <- fwf_positions(start = dicio_dom$posicao_inicial, 
                           end = dicio_dom$end, 
                           col_names = dicio_dom$codigo)

unzip(paste0(pathdir,"PNAD/2013/Dados_20170807.zip"),exdir = paste0(pathdir,"PNAD/2013/"))
pnad_2013 <- read_fwf(paste0(pathdir,"PNAD/2013/Dados/DOM2013.txt"), 
                      col_positions = col_specs, 
                      col_types = cols(.default = "c"))

#Etapa 3: Processamento -------------------------------------------------------

# Altera classe das colunas
pnad_2013 <- pnad_2013 %>%
  mutate(
    V0105 = as.numeric(V0105),   # Total de moradores
    V4623A = as.numeric(V4623A),  # Segurança alimentar
    V4611 = as.numeric(V4611),    # Peso do domicílio
    V4602 = as.numeric(V4602)    # Estrato
  )


# Criar uma nova variável categórica para facilitar a análise
pnad_2013 <- pnad_2013 %>%
  mutate(seguranca_alimentar = case_when(
    V4623A %in% c(1, 6) ~ "Segurança Alimentar",
    V4623A %in% c(2, 7) ~ "Insegurança Alimentar Leve",
    V4623A %in% c(3, 8) ~ "Insegurança Alimentar Moderada",
    V4623A %in% c(4, 9) ~ "Insegurança Alimentar Grave",
    TRUE ~ "Não Aplicável"
  ))

pnad_2013 <- pnad_2013 %>%
  mutate(situacao_domicilio = case_when(
    V4105 %in% c(1, 2, 3) ~ "Urbano",
    V4105 %in% c(4, 5, 6, 7, 8) ~ "Rural"
  ))


# Gerando o dado para o Brasil
pnad_2013_brasil <- pnad_2013 %>%
  mutate(Regiao = "Brasil") %>%
  group_by(Regiao, seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V0105*V4611, na.rm=TRUE),
            num_domicilios = sum(V4611, na.rm = TRUE)) 


# Gerando o dado para os estados
pnad_2013_estados <- pnad_2013 %>%
  group_by(UF,seguranca_alimentar,situacao_domicilio) %>% 
  summarise(num_pessoas = sum(V0105*V4611, na.rm=TRUE),
            num_domicilios = sum(V4611, na.rm = TRUE)) %>% 
  rename(Regiao = UF)

# Juntando os dois dataframes
pnad_2013_completo <- bind_rows(pnad_2013_brasil, pnad_2013_estados)

# Substituindo os códigos pelos nomes dos estados usando case_when
pnad_2013_completo <- pnad_2013_completo %>%
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
head(pnad_2013_completo)


write.table(pnad_2013_completo, paste(outdir,"tabela_seguranca_alimentar_pnad_2013_26marco2025.csv", sep = ""),row.names = F, sep = ";")



# Calculando totais para as porcentagens
totais <- sumarizacao_segalimentar %>%
  group_by(situacao_domicilio) %>%
  summarise(
    total_pessoas = sum(num_pessoas, na.rm = TRUE),
    total_domicilios = sum(num_domicilios, na.rm = TRUE)
  )

# Unindo os totais com os dados principais
dados_completos <- sumarizacao_segalimentar %>%
  left_join(totais, by = "situacao_domicilio") %>%
  mutate(
    percentual_pessoas = round((num_pessoas / total_pessoas) * 100, 1),
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

# Reestruturando a tabela
tabela_formatada <- dados_completos %>%
  filter(seguranca_alimentar != "Não Aplicável") %>%
  select(seguranca_alimentar, situacao_domicilio, num_pessoas, percentual_pessoas, num_domicilios, percentual_domicilios) %>%
  pivot_wider(
    names_from = situacao_domicilio,
    values_from = c(num_pessoas, percentual_pessoas, num_domicilios, percentual_domicilios),
    names_glue = "{situacao_domicilio}_{.value}"
  )

# Visualizando a tabela
print(tabela_formatada)


library(dplyr)
library(tidyr)

# Calculando totais para as porcentagens
totais <- sumarizacao_segalimentar %>%
  group_by(situacao_domicilio) %>%
  summarise(
    total_pessoas = sum(num_pessoas, na.rm = TRUE),
    total_domicilios = sum(num_domicilios, na.rm = TRUE)
  )

# Unindo os totais com os dados principais
dados_completos <- sumarizacao_segalimentar %>%
  left_join(totais, by = "situacao_domicilio") %>%
  mutate(
    percentual_pessoas = round((num_pessoas / total_pessoas) * 100, 1),
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

# Criando a linha de Insegurança Alimentar Total
inseguranca_total <- dados_completos %>%
  filter(grepl("Insegurança Alimentar", seguranca_alimentar)) %>%
  group_by(situacao_domicilio) %>%
  summarise(
    seguranca_alimentar = "Insegurança Alimentar",
    num_pessoas = sum(num_pessoas),
    num_domicilios = sum(num_domicilios),
    total_pessoas = first(total_pessoas),
    total_domicilios = first(total_domicilios)
  ) %>%
  mutate(
    percentual_pessoas = round((num_pessoas / total_pessoas) * 100, 1),
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

# Combinando os dados com a linha de Insegurança Alimentar Total
dados_completos_final <- bind_rows(dados_completos, inseguranca_total)

# Reestruturando a tabela
tabela_formatada <- dados_completos_final %>%
  filter(seguranca_alimentar != "Não Aplicável") %>%
  select(seguranca_alimentar, situacao_domicilio, num_pessoas, percentual_pessoas, num_domicilios, percentual_domicilios) %>%
  pivot_wider(
    names_from = situacao_domicilio,
    values_from = c(num_pessoas, percentual_pessoas, num_domicilios, percentual_domicilios),
    names_glue = "{situacao_domicilio}_{.value}"
  ) %>%
  arrange(factor(seguranca_alimentar, levels = c("Insegurança Alimentar Leve", "Insegurança Alimentar Moderada", "Insegurança Alimentar Grave", "Insegurança Alimentar", "Segurança Alimentar")))

# Visualizando a tabela
print(tabela_formatada)

