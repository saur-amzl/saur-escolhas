# ------------------------------------------------------------------------------
# Script para processar os dados da RAIS
# ------------------------------------------------------------------------------

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/raw/", sep = "/")
outdir <-  paste(path, "data/outputs", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")


## Etapa 1: Leitura dos dados da RAIS e ajustes iniciais no dado ----------------------

rais <- read.table(paste0(pathdir, "RAIS/RAIS_ESTAB_PUB.txt"), sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

cnae_descricao <- readxl::read_excel(paste0(dicdir,"/dicio_cnae_descricao.xlsx"),sheet = 'Sheet1', range = "A1:D22")
cnae_descricao <- cnae_descricao %>% filter(cnae_subclasse>1) %>% mutate(cnae_subclasse = as.integer(cnae_subclasse))

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

# Etapa Checagem ---------------------------------------------------------------

rais_cnae <- rais %>% 
  dplyr::filter(`CNAE.2.0.Subclasse` %in% cnae_descricao$cnae_subclasse) %>%
  dplyr::filter((`Ind.Atividade.Ano` == 1.0)) 

confere <- rais_cnae %>% group_by(`CNAE.2.0.Subclasse`) %>% 
  summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))

confere <- rais %>% filter(`Município` == 130090) %>% 
  dplyr::filter(`CNAE.2.0.Subclasse` %in% cnae_descricao$cnae_subclasse) %>% 
  dplyr::filter(Ind.Rais.Negativa == 0 | (Ind.Rais.Negativa ==1 & Qtd.Vínculos.Ativos >0))
confere1 <- rais %>% filter(`Município` == 130090) %>% 
  dplyr::filter(`CNAE.2.0.Subclasse` %in% cnae_descricao$cnae_subclasse) %>% 
  dplyr::filter(Ind.Rais.Negativa == 0 | (Ind.Rais.Negativa ==1 & Qtd.Vínculos.Ativos >0))


confere2 <- rais %>% 
  dplyr::filter(`CNAE.2.0.Subclasse` == 5611202) %>% 
  dplyr::filter((`Ind.Atividade.Ano` == 1.0)) 


## Etapa 2: Filtra os dados ----------------------------------------------------
# Seleciona os dados com os cnaes de interessse, de empresas ativas, dos estados
# de interesse
rais_cnae <- rais %>% 
  dplyr::filter(`CNAE.2.0.Subclasse` %in% cnae_descricao$cnae_subclasse) %>%
  dplyr::filter((`Ind.Atividade.Ano` == 1.0)) %>%
  dplyr::filter(`UF` %in%  c("11","12","13","14","15","16","17","21","51")) %>%
  rename(Municipio = `Município`) %>%
  mutate(Municipio =  as.character(Municipio))

# estado da amzl
rais_cnae_es <- rais_cnae %>%
  left_join(cnae_descricao, by = c(`CNAE.2.0.Subclasse`="cnae_subclasse")) %>%
  rename(sigla_uf = `UF`) 
  

# municípios de interesse
rais_cnae_mun <- rais_cnae %>%
  dplyr::filter(Municipio%in% all_rm_ri$cd_mun6) %>%
  left_join(all_rm_ri, by = c("Municipio"="cd_mun6")) %>%
  left_join(cnae_descricao, by = c(`CNAE.2.0.Subclasse`="cnae_subclasse"))

## Etapa 3: Sumarização dos dados ----------------------------------------------
sumarizacao_rais_municipios <- rais_cnae_mun %>% 
  group_by(sigla_uf,nm_rm,cd_mun,nm_mun,`CNAE.2.0.Subclasse`,descricao_cnae_subclasse) %>%
  summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))


sumarizacao_rais_estados <- rais_cnae_es %>% 
  group_by(sigla_uf,Municipio,`CNAE.2.0.Subclasse`,descricao_cnae_subclasse) %>%
  summarise(num_estb=n(),
            qtidade_vinculos_clt = sum(`Qtd.Vínculos.CLT`,na.rm = TRUE),
            qtidade_vinculos_ativos = sum(`Qtd.Vínculos.Ativos`,na.rm = TRUE))



# Etapa 4: Salva os dados ---------------------------------------------------------
# Criar um arquivo Excel
wb <- createWorkbook()

# Adicionar a aba para a primeira tabela
addWorksheet(wb, "mun_estabrais")
writeData(wb, "mun_estabrais", sumarizacao_rais_municipios)


# Salvar o arquivo
saveWorkbook(wb, file.path(outdir, "tabela_rais_mun_19marco2025.xlsx"), overwrite = TRUE)

write.table(sumarizacao_rais_municipios, paste0(outdir,"/tabela_rais_mun_19marco2025.csv", sep = ""),row.names = F, sep = ";")
write.table(sumarizacao_rais_estados, paste0(outdir,"/tabela_rais_estamzl_19marco2025.csv", sep = ""),row.names = F, sep = ";")

