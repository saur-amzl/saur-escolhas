## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

## As tabelas geradas neste script pretendem investigar 
##Alteração no consumo de alimentos e Alteração no consumo de alimentos (FVL)	
# Atrabés da tabela de Aquisição alimentar domiciliar per capita anual (Quilogramas) para os anos de 2008 e 2018

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")

# Etapa 1: Leitura dos dados ---------------------------------------------------
#2007-2008
base_aquisicao_alimentar_2008 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof0708.csv"),sep = ";")

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")

#Tabela com a lista de todos os produtos
allprodutos_2008 <- 
  readxl::read_excel(paste0(pathdir,"2007-2008/Documentacao_20231009/Cadastro_de_Produtos.xlsx"),sheet = 'TESTE', range= "A7:D13785",
  col_types = c("text", "text", "text", "text"))  # Exemplo de tipos
colnames(allprodutos_2008) <- c("quadro_2008","grupo_2008","codigo_2008","produto_2008")

allprodutos_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Documentacao_20230713/Cadastro_de_Produtos.xls"),sheet = 'T_SAS_PRODUTOS_FINAL', range= "A1:C13475",
                     col_types = c("text", "text", "text"))
colnames(allprodutos_2018) <- c("quadro_2018","codigo_2018","produto_2018")

# Tabela com Tradutor_Alimenta‡ֶo
tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"2007-2008/Tradutores_das_Tabelas_20231009/POF_2008-2009_Codigos_de_alimentacao.xls"),sheet = 'Componentes', range= "A3:G2342",
                     col_types = c("text", "text", "text","text","text", "text", "text"))
colnames(tradutor_alimentacao_2008) <- c("codigo", "nivel1", "descr1","nivel2","descr2", "nivel3", "descr3")

tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Alimenta‡ֶo.xls"),sheet = 'Planilha1', range= "A1:I2694",
                     col_types = c("text", "text", "text","text","text", "text", "text","text","text"))
colnames(tradutor_alimentacao_2018) <- c("codigo","nivel0", "descr0","nivel1", "descr1","nivel2","descr2", "nivel3", "descr3")

# Tabela com Tradutor de Aquisição Alimentar - Só tem disponível para 2018
tradutor_aquisicao_alimentar_2018 <-
  readxl::read_excel(paste0(pathdir,"2017_2018/Tradutores_20230713/Tradutor_Aquisi‡ֶo_Alimentar.xls"))
colnames(tradutor_aquisicao_alimentar_2018) <- c("codigo_aqui","nivel1_aqui", "descr1_aqui","nivel2_aqui","descr2_aqui", "nivel3_aqui", "descr3_aqui")
tradutor_aquisicao_alimentar_2018$codigo_aqui <- as.character(tradutor_aquisicao_alimentar_2018$codigo_aqui)

# tabela com classificação da NOVA
classificacao_nova_2018 <-
  readxl::read_excel(paste0(pathdir,"classificacao_nova_fromaquisicaogrupos.xlsx"),sheet = 'Sheet1', range= "A1:D331",
                     col_types = c("text", "text", "text","text"))
colnames(classificacao_nova_2018)

classificacao_nova_2018 <- classificacao_nova_2018 %>%
  group_by(cod_2018) %>%
  slice(1) # Seleciona a primeira ocorrência para cada `cod_2018`

# Tabela Classificação - Escolhas
classificacao_escolhas_2018 <-
  readxl::read_excel(paste0(pathdir,"Cadastro de Produtos POF 2019 - Contribuição.xlsx"),sheet = 'Planilha1', range= "A1:D8322",
                     col_types = c("text", "text", "text","text"))
colnames(classificacao_escolhas_2018) <- c("quadro","V9001","desc_tbescolhas","class_tbescolhas")


# Tabela com classificação MANUAL dos produtos regionais
alimentos_regionais_2008 <- 
  readxl::read_excel(paste0(pathdir,"tabela_produtos_aquisicao_alimentos_regionais.xlsx"),sheet = '2008', range= "A1:E253",
                     col_types = c("text", "text", "text","text","text"))


alimentos_regionais_2018 <- 
  readxl::read_excel(paste0(pathdir,"tabela_produtos_aquisicao_alimentos_regionais.xlsx"),sheet = '2018', range= "A1:E240",
                     col_types = c("text", "text", "text","text","text"))


## Processamento

# Seleciona apenas os alimentos indicados na tabela de aquisição da POF 2008
produtos_aquisicao_2008 <- allprodutos_2008 %>%
  mutate(codigo_2008_comp = ifelse(is.na(grupo_2008), paste0(quadro_2008, codigo_2008), paste0(grupo_2008, codigo_2008)),
         codigo_2008_comp = as.integer(codigo_2008_comp)) %>%
  right_join(base_aquisicao_alimentar_2008, by = c('codigo_2008_comp' = 'V9001')) %>%
  select(grupo_2008,quadro_2008,codigo_2008,codigo_2008_comp,produto_2008) %>%
  distinct() %>%
  mutate(codigo = trunc(codigo_2008_comp/100)) %>% 
  mutate(codigo = as.character(codigo)) %>%
  left_join(tradutor_alimentacao_2008, by = c('codigo')) %>% 
  left_join(tradutor_aquisicao_alimentar_2018, by = c('codigo'="codigo_aqui")) %>% 
  mutate(codigo_2008_comp = as.character(codigo_2008_comp)) %>%
  left_join(alimentos_regionais_2008, by = c('codigo_2008_comp')) %>%
  left_join(classificacao_escolhas_2018 %>% select(V9001,desc_tbescolhas,class_tbescolhas), by = c('codigo_2008_comp' = 'V9001')) %>%
  mutate(class_analisegeral = ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas", "Legumes e verduras") & 
                                     descr2_aqui == "Leguminosas", "Leguminosa",
                                   ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas") & 
                                            descr2_aqui == "Cereais", "Cereais",
                                          ifelse(descr2 %in% c("Tubérculos e raízes"), "Tubérculos e raízes",
                                                 ifelse(descr2 %in% c("Legumes e verduras") & 
                                                          descr1_aqui %in% c("Hortaliças", "Frutas"), "Legumes e verduras",
                                                        ifelse(descr2 %in% c("Frutas"), "Frutas",
                                                               ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas") & 
                                                                        descr2_aqui == "Castanhas e nozes", "Castanhas e nozes",
                                                                      ifelse(descr2 %in% c("Carnes, vísceras e pescados", "Aves e ovos") & 
                                                                               descr2_aqui != "Alimentos preparados", "Carnes e Ovos",
                                                                             ifelse(descr2 %in% c("Leites e derivados") & 
                                                                                      descr1_aqui == "Laticínios", "Leites e Queijos",
                                                                                    ifelse(nivel2_aqui %in% c("Açúcares", "Gorduras", "Óleos", "Sais"), "Açúcares, sal, óleos e gorduras",
                                                                                           ifelse(nivel2_aqui %in% c("Cafés", "Chás"), "Café e Chá", "Outros")))))))))),
       class_analisegeral = ifelse(class_tbescolhas == 4, "Ultraprocessados", class_analisegeral))

# Seleciona apenas os alimentos indicados na tabela de aquisição da POF 2018
produtos_aquisicao_2018 <- allprodutos_2018 %>%
  mutate(codigo_2018 = as.integer(codigo_2018)) %>%
  right_join(base_aquisicao_alimentar_2018, by = c('codigo_2018' = 'V9001')) %>%
  select(quadro_2018,codigo_2018,produto_2018) %>%
  distinct() %>%
  mutate(codigo = trunc(codigo_2018/100)) %>% 
  mutate(codigo = as.character(codigo)) %>%
  left_join(tradutor_alimentacao_2018, by = c('codigo')) %>%
  left_join(tradutor_aquisicao_alimentar_2018, by = c('codigo'="codigo_aqui")) %>% 
  left_join(classificacao_nova_2018, by = c('codigo' ='cod_2018')) %>%
  mutate(codigo_2018 = as.character(codigo_2018)) %>%
  left_join(alimentos_regionais_2018, by = c('codigo_2018')) %>%
  left_join(classificacao_escolhas_2018 %>% select(V9001,desc_tbescolhas,class_tbescolhas), by = c('codigo_2018' = 'V9001')) %>%
  mutate(class_analisegeral = ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas", "Legumes e verduras") & 
                                       descr2_aqui == "Leguminosas", "Leguminosa",
                                     ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas") & 
                                              descr2_aqui == "Cereais", "Cereais",
                                            ifelse(descr2 %in% c("Tubérculos e raízes"), "Tubérculos e raízes",
                                                   ifelse(descr2 %in% c("Legumes e verduras") & 
                                                            descr1_aqui %in% c("Hortaliças", "Frutas"), "Legumes e verduras",
                                                          ifelse(descr2 %in% c("Frutas"), "Frutas",
                                                                 ifelse(descr2 %in% c("Cereais, leguminosas e oleaginosas") & 
                                                                          descr2_aqui == "Castanhas e nozes", "Castanhas e nozes",
                                                                        ifelse(descr2 %in% c("Carnes, vísceras e pescados", "Aves e ovos") & 
                                                                                 descr2_aqui != "Alimentos preparados", "Carnes e Ovos",
                                                                               ifelse(descr2 %in% c("Leites e derivados") & 
                                                                                        descr1_aqui == "Laticínios", "Leites e Queijos",
                                                                                      ifelse(nivel2_aqui %in% c("Açúcares", "Gorduras", "Óleos", "Sais"), "Açúcares, sal, óleos e gorduras",
                                                                                             ifelse(nivel2_aqui %in% c("Cafés", "Chás"), "Café e Chá", "Outros")))))))))),
    class_analisegeral = ifelse(class_tbescolhas == 4, "Ultraprocessados", class_analisegeral))

  
  
  

colnames(produtos_aquisicao_2018)
write.table(produtos_aquisicao_2018, paste("tabela_produtos_aquisicao_2018_29jan25_v2.csv", sep = ""),row.names = F, sep = ";")
write.table(produtos_aquisicao_2008, paste("tabela_produtos_aquisicao_2008_29jan25_v2.csv", sep = ""),row.names = F, sep = ";")

write.table(produtos_aquisicao_2008_compara, paste("tabela_produtos_aquisicao_2008_jan25_compara.csv", sep = ""),row.names = F, sep = ";")

