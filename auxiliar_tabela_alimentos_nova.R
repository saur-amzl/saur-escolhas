
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)

library(stringr)

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


mapeamento_nova_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_pof2018_nova_final.xlsx"),sheet = 'mapeamento', range= "A1:E8801")

classificacao_grupo_consumo_alimentar_2018  <-
  readxl::read_excel(paste0(pathdir,"classificacao_grupo_consumo_alimentar.xlsx"),sheet = 'Sheet1', range= "A1:D1594")

tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Alimenta‡ֶo.xls"),sheet = 'Planilha1', range= "A1:I2694")
colnames(tradutor_alimentacao_2018)

tabela_mapeamento <- mapeamento_nova_alimentacao_2018 %>%
  left_join(classificacao_grupo_consumo_alimentar_2018, by = c("codigo_2018"="codigo_item"))

tabela_mapeamento <- tabela_mapeamento %>% mutate(cod_trunc = trunc(codigo_2018/100)) %>%
  left_join(tradutor_alimentacao_2018, by = c("cod_trunc"="Codigo"))


write.table(tabela_mapeamento, paste(pathdir,"mapeamento_pof2018_nova_final_confere.csv", sep = ""),row.names = F, sep = ";")



#2008
base_aquisicao_alimentar_2008 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof0708.csv"),sep = ";")
base_uc_2008 <- read.csv(paste0(pathdir,"tabela_base_uc_pof0708.csv"),sep = ";")

# Selecionando apenas estratos de interesse: AMZLEGAL
#2007-2008
base_aquisicao_alimentar_2008_amzl <- base_aquisicao_alimentar_2008 %>%
  right_join(tabela_estratos_2008, by = c("COD_UF", "ESTRATO_POF"))

base_uc_2008_amzl <- base_uc_2008 %>%
  right_join(base_uc_2008, by = c("COD_UF", "ESTRATO_POF"))

todos_produtos_2008 <-
  readxl::read_excel(paste0(pathdir,"2007-2008/Documentacao_20231009/Cadastro de Produtos.xls"),sheet = 'TESTE', range = "A7:D13785")

todos_produtos_2008 <- todos_produtos_2008 %>%
  mutate(V9001 = ifelse(is.na(GRUPO),str_c(QUADRO,CÓDIGO),str_c(GRUPO,CÓDIGO)))

produtos_alimentos_2008 <- read.csv("/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/produtos_alimentos_pof2008.csv")
colnames(produtos_alimentos_2008)


alimentos_amzl_08 <- base_aquisicao_alimentar_2008_amzl %>% select(V9001) %>% distinct()
base_2008_amzl <- alimentos_amzl_08 %>%
  left_join(produtos_alimentos_2008, by = c("V9001"="codigo_2008"))


mapeamento_nova_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_alimentacao_nova_br.xlsx"),sheet = 'Sheet1', range= "A1:Q8801")
colnames(mapeamento_nova_alimentacao_2018)
colnames(base_2008_amzl)

confere <- base_2008_amzl %>%
  left_join(mapeamento_nova_alimentacao_2018,by = c("V9001" ="codigo_2018")) %>%
  mutate(check = ifelse(descricao_produto_08 == descricao_produto, 'desc_igual',
                               ifelse(descricao_produto_08 != descricao_produto, 'desc_dif','nao_match')))


write.table(confere, paste(pathdir,"mapeamento_pof2008_nova_confere.csv", sep = ""),row.names = F, sep = ";")

