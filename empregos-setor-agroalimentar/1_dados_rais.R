# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl,openxlsx)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")


## Etapa 1: Leitura dos dados da RAIS e ajustes iniciais no dado ----------------------
rais <- read.table(
  paste0(pathdir, "RAIS/2023/RAIS_ESTAB_PUB.txt"), 
  sep = ";", 
  header = TRUE, 
  stringsAsFactors = FALSE, 
  fileEncoding = "latin1",
  colClasses = c("CNAE.2.0.Classe" = "character") 
)
#Listas de RM e RI, 
rm <- readxl::read_excel(paste0(dicdir,"/regioes_interesse.xlsx"),sheet = 'regiao_metropolitana', range = "A1:P138")
rm <-rm[c(2,3,9, 13,14)]
colnames(rm) <- c("cd_uf","sigla_uf","nm_rm","cd_mun","nm_mun")

ri <- readxl::read_excel(paste0(dicdir,"/regioes_interesse.xlsx"),sheet = 'regiao_imediata', range = "A1:F8")
ri <-ri[c(6,2,1)]
colnames(ri) <- c("nm_rm","cd_mun","nm_mun")

ri <- ri %>% mutate(cd_uf = 12,
                    sigla_uf = 'AC') %>% 
  relocate(cd_uf, sigla_uf,nm_rm,cd_mun,nm_mun)
all_rm_ri <- rbind(rm,ri)
all_rm_ri <- all_rm_ri %>% mutate(cd_mun6 = substr(cd_mun, 1, 6))


# página 23 do relatório parte 1: https://escolhas.org/publicacao/agricultura-urbana-em-belem/

# CNAE 2.0, especificamente as seções,
# Produção primária ou agropecuária: 
# Seção A (Agricultura, Pecuária, Produção Florestal, Pesca e Aquicultura)
# Comércio de alimentos: Grupos 46.2 (Com Matérias-primas Agrícolas e Animais 
# Vivos), 46.3 (Comércio de Produtos Alimentícios, Bebidas e Fumos), 47.1 
# (Supermercado e Hipermercado) e 56.1 (Comércio Ambulante e Feiras);
# Serviços alimentares: Divisão 56 (Alimentação) da Seção I (Alojamento e Alimentação)

cnae_agroalimentar <- readxl::read_excel(paste0(dicdir,"/mapeamento_classe_cnae_setoragro_24marco2025.xlsx"),sheet = 'map_cnae_rais', range = "A1:C92")
cnae_agroalimentar <- cnae_agroalimentar %>% mutate(classe_new = gsub("[.-]", "", Classe))

uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
  "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "Es", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

## Etapa 2:  ------------------
# Criar uma nova coluna identificando se o CNAE começa com algum dos códigos
rais_agroalimentar <- rais  %>% 
     dplyr::filter((`Ind.Atividade.Ano` == 1.0)) %>%
     dplyr::filter(`CNAE.2.0.Classe` %in% cnae_agroalimentar$classe_new) %>%
     left_join(cnae_agroalimentar, by = c(`CNAE.2.0.Classe`="classe_new"))


# municípios de interesse
rais_agroalimentar_mun <- rais_agroalimentar %>%
  dplyr::filter(`UF` %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  mutate(estado = recode(`UF`, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%
  rename(Municipio = `Município`) %>%
  mutate(Municipio =  as.character(Municipio)) %>% 
  dplyr::filter(Municipio%in% all_rm_ri$cd_mun6) %>%
  left_join(all_rm_ri, by = c("Municipio"="cd_mun6"))



total_brasil <- rais_agroalimentar %>%
     group_by(cadeia)  %>%
     summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))

total_estados <- rais_agroalimentar %>%
  dplyr::filter(`UF` %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  mutate(estado = recode(`UF`, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%
  group_by(`UF`,estado,cadeia)  %>%
  summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))  
  
total_mun_rm <- rais_agroalimentar_mun %>%
  group_by(estado,nm_rm,cd_mun,nm_mun,cadeia)  %>%
  summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))   


# Criar um arquivo Excel
wb <- createWorkbook()

# Adicionar a aba para a primeira tabela
addWorksheet(wb, "estabelecimentos_brasil")
writeData(wb, "estabelecimentos_brasil", total_brasil)

addWorksheet(wb, "estabelecimentos_estados_amzl")
writeData(wb, "estabelecimentos_estados_amzl", total_estados)


addWorksheet(wb, "estabelecimentos_mun_rm_amzl")
writeData(wb, "estabelecimentos_mun_rm_amzl", total_mun_rm)

# Salvar o arquivo
saveWorkbook(wb, file.path(outdir, "tabela_rais_setor_agroalimentar_24marco2025.xlsx"), overwrite = TRUE)

