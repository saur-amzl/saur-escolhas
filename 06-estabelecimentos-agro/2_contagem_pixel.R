

# Limpar area de trabalho
rm(list = ls())

# Lista de pacotes necessários
library("raster")
library("data.table")
library("RSQLite")
library("dplyr")

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
intdir <-  paste(path, "data/intermediate/", sep = "/")

mun = raster(paste0(intdir,'pa_br_municipios_2023_30m.tif')); mun;
areas_urbanizadas =  raster(paste0(intdir,'AU_2022_AreasUrbanizadas2019_Brasil_30m.tif')); areas_urbanizadas;
uso = raster(paste0(intdir,'brasil_coverage_2023.tif')); uso;
cnefe = raster(paste0(intdir,'re_amzl_cnefe_estab_agropecuario_30m.tif')); cnefe;

# Etapa 2 : Contagem de Pixel
output_file <- 'contagem_pixel_analise_30m.db' 
if (file.exists(paste(intdir, output_file, sep = ""))) file.remove(paste(intdir, output_file, sep = ""))
conn <- dbConnect(SQLite(), paste(intdir, output_file, sep = ""))

bss <- blockSize(mun)

process_block <- function(i) {
  print(paste("Processando bloco", i))
  
  x <- data.table(
    mun = as.integer(getValues(mun, row = bss$row[i], nrows = bss$nrows[i])),
    uso = as.integer(getValues(uso, row = bss$row[i], nrows = bss$nrows[i])),
    areas_urbanizadas = as.integer(getValues(areas_urbanizadas, row = bss$row[i], nrows = bss$nrows[i])),
    cnefe = as.integer(getValues(cnefe, row = bss$row[i], nrows = bss$nrows[i]))
  )
  
  # Verifica a estrutura de x
  if (!is.data.table(x)) {
    x <- as.data.table(x)
  }
  
  # Substituir valores de uso == 0 por NA
  x[, uso := fifelse(uso == 0, NA_integer_, uso)]
  
  
  # Classificar valores de uso do solo - Mapbiomas Col. 8
  x[, uso := ifelse(uso %in% c(39, 20, 40, 62, 41, 46, 47, 35, 48), 1,  # Agricultura
                    ifelse(uso == 9, 2,  # Silvicultura
                           ifelse(uso == 15, 3,  # Pastagem
                                  ifelse(uso == 21, 4,  # Moisaco
                                  ifelse(uso %in% c(3, 4, 5, 6, 11, 12, 32, 29, 49, 50), 5,  # Vegetação Nativa
                                         ifelse(uso == 24, 6,  # area urbanizada
                                         ifelse(uso %in% c(33,31), 7, 8)))))))  # Corpos d'água / Outros
  ]
  
  # Agrupamento e cálculo de área
  resultado <- x %>%
    group_by(mun,areas_urbanizadas,uso,cnefe) %>%
    summarise(npixel = n() , .groups = "drop")
  
  # Inserir dados na tabela
  dbWriteTable(conn, 'table_contagem_pixel', resultado, row.names = FALSE, append = TRUE)} 



# Aplica a função 'process_block' para cada bloco
system.time(lapply(1:bss$n, process_block))

#Desconecta do Banco SQLite
dbDisconnect(conn)

