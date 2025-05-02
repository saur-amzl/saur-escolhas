# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl,openxlsx)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
inputdir <-  paste(path, "data/inputs", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")

pathdir <- paste(path, "data/raw/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")
intdir <-  paste(path, "data/intermediate/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")

uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
  "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "Es", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

# Etapa 1: Leitura dos dados ----------------------------------------------
#2017-2018
estabelecimentos_pof <- read.csv(paste0(outdir,"tabela_pof2018_guia_locais_porestado_19marco2025.csv"),sep = ";")
rais_estados <- read.csv(paste0(outdir,"tabela_rais_estamzl_19marco2025.csv"),sep = ";")
rais_municipios <- read.csv(paste0(outdir,"tabela_rais_mun_19marco2025.csv"),sep = ";")


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


# Classifica estabelecimentos --------------------------------------------------
estab_classificados <- estabelecimentos_pof %>%
  filter(cnae_subclasse > 1 & !is.na(cnae_subclasse)) %>%
  group_by(estado,cnae_subclasse) %>%
  mutate(classe = ifelse(perc_In_natura >= 0.50,'innatura',
                         ifelse(perc_Ultraprocessado >=0.50,"ultraprocessado","mistos")))


# Tabela para mapa -------------------------------------------------------------
aux_mapa <- rais_estados %>% 
  rename(cd_uf = sigla_uf) %>% 
  mutate(estado = recode(cd_uf, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>% # Caso não encontre, atribui "Desconhecido"
  full_join(estab_classificados %>% select(estado,cnae_subclasse,classe), by =c("estado","CNAE.2.0.Subclasse" = "cnae_subclasse"))

confere <- aux_mapa %>% filter(estado == 'MT')

input_mapa <- aux_mapa %>%
  group_by(cd_uf,Municipio,classe) %>%
  summarise(est_total = sum(num_estb,na.rm = TRUE))

input_mapa_wide <- input_mapa %>%
  pivot_wider(names_from = classe, values_from = est_total, values_fill = list(est_total = 0))

write.table(input_mapa_wide, paste0(outdir,"/input_mapa_numest_classificados_19marco2025.csv", sep = ""),row.names = F, sep = ";")

percentis <- input_mapa %>%
  group_by(classe) %>%
  summarise(
    p25 = quantile(est_total, 0.25, na.rm = TRUE),
    p50 = quantile(est_total, 0.50, na.rm = TRUE),  # Mediana
    p75 = quantile(est_total, 0.75, na.rm = TRUE),
    p99 = quantile(est_total, 0.99, na.rm = TRUE),
    p100 = quantile(est_total, 1.0, na.rm = TRUE),
  )

# Visualizar resultado
print(percentis)



# Tabelas para relatório --------------------------------------------------------
tab_relatorio <- estab_classificados %>%
  select(estado,cnae_subclasse,descricao_cnae_subclasse,classe)  %>%
  group_by(estado, descricao_cnae_subclasse, classe) %>%
  pivot_wider(names_from = estado, values_from = classe)

# Substituindo NA por string vazia para evitar erros no Excel
tab_relatorio[is.na(tab_relatorio)] <- ""

# Criando o arquivo Excel
wb <- createWorkbook()
addWorksheet(wb, "tab_locais_pof_classificados")

# Escrevendo os dados na planilha
writeData(wb, "tab_locais_pof_classificados", tab_relatorio)

# Criando estilos de cores
style_innatura <- createStyle(fgFill = "#90EE90")  # Verde
style_mistos <- createStyle(fgFill = "#FFFF00")    # Amarelo
style_ultraprocessado <- createStyle(fgFill = "#FF6347") # Vermelho

# Aplicando cores diretamente às células (sem formatação condicional)
for (col in 2:ncol(tab_relatorio)) {  # Começa na 2ª coluna (UFs)
  for (row in 1:nrow(tab_relatorio)) {
    valor <- tab_relatorio[row, col]
    if (valor == "innatura") {
      addStyle(wb, "tab_locais_pof_classificados", style = style_innatura, rows = row + 1, cols = col, gridExpand = TRUE)
    } else if (valor == "mistos") {
      addStyle(wb, "tab_locais_pof_classificados", style = style_mistos, rows = row + 1, cols = col, gridExpand = TRUE)
    } else if (valor == "ultraprocessado") {
      addStyle(wb, "tab_locais_pof_classificados", style = style_ultraprocessado, rows = row + 1, cols = col, gridExpand = TRUE)
    }
  }
}

# Salvando o arquivo Excel
saveWorkbook(wb, file.path(outdir, "tabela_pof2018_locais_classificados_19marco2025.xlsx"), overwrite = TRUE)

## SEGUNDA TABELA

tab_relatorio2 <- aux_mapa %>% 
  full_join(all_rm_ri %>% mutate(cd_mun6 = as.integer(cd_mun6)), by = c("Municipio"="cd_mun6"))


por_estado <- tab_relatorio2 %>% 
    filter(!is.na(classe)) %>%
    group_by(estado,classe) %>% 
    summarise(total_estab = sum(num_estb, na.rm = TRUE))
    

por_regiao_metropolitana <- tab_relatorio2 %>% 
  filter(!is.na(classe)) %>%
  group_by(estado,nm_rm,classe) %>% 
  summarise(total_estab = sum(num_estb, na.rm = TRUE))

# Criando o arquivo Excel
wb <- createWorkbook()
addWorksheet(wb, "tab_locais_pof_estados")
writeData(wb, "tab_locais_pof_estados", por_estado)

addWorksheet(wb, "tab_locais_pof_regmetrop")
writeData(wb, "tab_locais_pof_regmetrop", por_regiao_metropolitana)

# Salvando o arquivo Excel
saveWorkbook(wb, file.path(outdir, "tabela_pof2018_locais_classificados_numestab_11abril2025.xlsx"), overwrite = TRUE)

