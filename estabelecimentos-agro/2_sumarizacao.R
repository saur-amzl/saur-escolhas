

# Limpar area de trabalho
rm(list = ls())

# Lista de pacotes necessários
library("raster")
library("data.table")
library("RSQLite")
library("dplyr")
library(openxlsx)

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
inputdir <-  paste(path, "data/inputs", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")

# Etapa 1: Leitura dos dados. --------------------------------------------------
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


uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
  "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "Es", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

conn <- dbConnect(SQLite(), paste(pathdir, 'contagem_pixel_analise_30m.db', sep = ""))
data <- dbGetQuery(conn, "SELECT * FROM table_contagem_pixel")

# Etapa 2: processamento -------------------------------------------------------
data <- data %>%
  mutate(cd_uf = substr(mun, 1, 2))

# Estados
cnefe_es_amzl <- data %>% 
  dplyr::filter(cd_uf %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  filter(!is.na(cnefe)) %>% 
  mutate(estado = recode(cd_uf, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido"))  %>%
  mutate(is_area_urb_ibge = ifelse(is.na(areas_urbanizadas), "NO",
                                   ifelse(areas_urbanizadas == 1, 'YES', "NO")),
         uso_mapbiomas = ifelse(is.na(uso),"Outros",
                                ifelse(uso==1,'Agricultura',
                                              ifelse(uso==3,'Pastagem',
                                                            ifelse(uso==5,"Vegetação Nativa",
                                                                   ifelse(uso==6,"Área Urbanizada",
                                                                          ifelse(uso == 7,"Corpos d'água","Outros"))))))) %>%
  group_by(estado,is_area_urb_ibge,uso_mapbiomas)%>%
  summarise(ptos_cenfe = sum(cnefe  * npixel, na.rm = TRUE))


# Municípios do RM
cnefe_mun_reamzl <- data %>% 
  mutate(mun = as.character(mun)) %>% 
  right_join(all_rm_ri %>% dplyr::select(sigla_uf,nm_rm,cd_mun,nm_mun),by = c("mun"="cd_mun")) %>% 
  mutate(is_area_urb_ibge = ifelse(is.na(areas_urbanizadas), "NO",
                                   ifelse(areas_urbanizadas == 1, 'YES', "NO")),
         uso_mapbiomas = ifelse(is.na(uso),"Outros",
                                ifelse(uso==1,'Agricultura',
                                       ifelse(uso==3,'Pastagem',
                                              ifelse(uso==5,"Vegetação Nativa",
                                                     ifelse(uso==6,"Área Urbanizada",
                                                            ifelse(uso == 7,"Corpos d'água","Outros"))))))) %>%
  group_by(sigla_uf,nm_rm,mun,nm_mun,is_area_urb_ibge,uso_mapbiomas)%>%
  summarise(ptos_cenfe = sum(cnefe  * npixel, na.rm = TRUE))



# Criando o arquivo Excel
wb <- createWorkbook()
addWorksheet(wb, "cnefe_es_amzl")
writeData(wb, "cnefe_es_amzl", cnefe_es_amzl)


addWorksheet(wb, "cnefe_mun_reamzl")
writeData(wb, "cnefe_mun_reamzl", cnefe_mun_reamzl)


# Salvando o arquivo Excel
saveWorkbook(wb, file.path(outdir, "tabela_estab_agropecuarios_censo2022_24marco2025.xlsx"), overwrite = TRUE)

