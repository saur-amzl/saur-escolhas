# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table,readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(openxlsx)
library("PNADcIBGE")

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
inputdir <- paste(path, "data/inputs/", sep = "/")
outdir <- paste(path, "data/outputs/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")

# Etapa 1: Leitura dos dados ----------------------------------------------------
#https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html?caminho=Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre
pnadc_2023 <- read_pnadc(microdata = paste0(pathdir,"PNADC/PNADC_2023_trimestre4.txt"),
                         input_txt = paste0(dicdir,"input_PNADC_trimestre4_20240425.txt"))

dados_pnadc <- pnadc_labeller(pnadc_2023, "/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-escolhas/data/PNADC/dicionario_PNADC_microdados_trimestre4_20240816.xls")

uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
  "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "Es", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

# página 23 do relatório parte 1: https://escolhas.org/publicacao/agricultura-urbana-em-belem/


cnae_agroalimentar <- readxl::read_excel(paste0(dicdir,"/mapeamento_classe_cnae_setoragro_24marco2025.xlsx"),sheet = 'map_cnae_pnadc', range = "A1:C53")


# Essas variáveis podem mudar de ano para ano (precisamos checar isso)
variaveis <- c("UF", "Capital", "RM_RIDE", "UPA", "Estrato",
               "V1008", # Número de seleção do domicílio
               "V1014", # Painel
               "V1022", # Situação do domicílio
               "V1023", # Tipo de área (identifica se é capital, resto da RM, resto do estado)
               "V1028", # Peso do domicílio e das pessoas com calibração
               "V2001", # Número de pessoas no domicílio
               "V2005", # Condição no domicílio (responsável é o um)
               "V2007", # Sexo (1= MASC; 2 = FEM)
               "V2009", # idade do morador
               "V2010", # Cor ou raça
               "V4013", # CNAE ativ principal
               "VD3004", # Escolaridade 
               "VD4001", # PEA
               "VD4002", # Ocupadas e não ocupadas
               "VD4004A", # Subocupadas
               "VD4005", # Desalentadas
               "VD4008", # Tipo de ocupação
               "VD4009", # Posição na ocupação
               "VD4010", # Atividade do Estabelecimento
               "VD4011", # Atividade dos trabalhadores
               "VD4013", # horas trabalhadas (todos os trabalhos)
               "VD4016", # Rendimento mensal habitual do trabalho principal
               "VD4019" # Rendimento mensal habitual todos os trabalhos 

)


pnad <- pnadc_2023[variaveis]; gc()
#`V1032`, `VD5007`, and `VD5008`
pnad <- pnad %>% 
  rename(unidade_geo = V1023, 
         num_pessoas_dom = V2001,
         cor_raca = V2010,
         sexo = V2007,
         cond_dom = V2005,
         escolaridade = VD3004,
         peso_dom = V1028,
         cnae = V4013,
         idade = V2009,
         PEA = VD4001,
         ocupadas = VD4002,
         subocupadas = VD4004A, # muda depois de 2017
         desalentadas = VD4005,
         tipo_ocupacap = VD4008,
         atividade_estabelecimentos = VD4010,
         atividade_trabalhadores = VD4011,
         renda_trab_princ = VD4016,
         renda_trab_todas_fontes = VD4019,
         horas_trabalhadas = VD4013
  ) %>% 
  mutate(sexo = case_when(
    sexo == "Homem" ~ "Masculino",
    sexo == "Mulher" ~ "Feminino", 
    .default = sexo))


pnad <- pnad %>%  
  mutate(d_agroalimentar = ifelse(cnae %in% cnae_agroalimentar$classe_cnae_pnadc, 1, 0 )) %>% 
  left_join(cnae_agroalimentar, by = c('cnae'= "classe_cnae_pnadc" )); gc() 
class(pnad$cnae)


total_brasil <- pnad %>% 
  filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
  group_by(cadeia)  %>%
  summarise(ocupados = sum(peso_dom,na.rm = TRUE))


total_estados <- pnad %>%
  dplyr::filter(`UF` %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  mutate(estado = recode(`UF`, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%
  filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
  group_by(`UF`,estado,cadeia)  %>%
  summarise(ocupados = sum(peso_dom,na.rm = TRUE))


total_brasil <- pnad %>% 
  filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
  group_by(cadeia)  %>%
  summarise(ocupados = sum(peso_dom,na.rm = TRUE))


total_capitais <- pnad %>%
  dplyr::filter(`UF` %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  mutate(estado = recode(`UF`, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%
  filter(!is.na(ocupadas), d_agroalimentar == 1 ) %>%
  filter(!is.na(Capital)) %>%
  group_by(`UF`,estado,Capital,cadeia)  %>%
  summarise(ocupados = sum(peso_dom,na.rm = TRUE))


# Criar um arquivo Excel
wb <- createWorkbook()

# Adicionar a aba para a primeira tabela
addWorksheet(wb, "ocupados_brasil")
writeData(wb, "ocupados_brasil", total_brasil)

addWorksheet(wb, "ocupados_estados_amzl")
writeData(wb, "ocupados_estados_amzl", total_estados)


addWorksheet(wb, "ocupados_capitais_amzl")
writeData(wb, "ocupados_capitais_amzl", total_capitais)

# Salvar o arquivo
saveWorkbook(wb, file.path(outdir, "tabela_pnadc_setor_agroalimentar_24marco2025.xlsx"), overwrite = TRUE)

