

#LOCAIS DE AQUISICAO DECRETO
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl,openxlsx)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
intdir <-  paste(path, "data/intermediate/", sep = "/")
outdir <-  paste(path, "data/outputs/", sep = "/")
dicdir <-  paste(path, "data/dic_map/", sep = "/")


source("set_estrato.R")

# Etapa 1: Leitura dos dados ----------------------------------------------
#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(intdir,"tabela_base_alimentacao_pof1718_10marco2025.csv"),sep = ";")
base_uc_2018 <- read.csv(paste0(intdir,"tabela_base_uc_pof1718_10marco2025.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(dicdir,"mapeamento_produtos_aquisicao_v16fevereiro25.xlsx"),sheet = '2018', range = "A1:AF4911")
tradutor_alimentacao_2018 <-tradutor_alimentacao_2018[c(2:4,22:25,30:32)]
colnames(tradutor_alimentacao_2018) <- c("codigo_2018_trad","produto_2018_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

tradutor_locais <-
  readxl::read_excel(paste0(dicdir,"/mapeamento_local_pof_rais_19marco2025.xlsx"),sheet = 'Sheet1', range= "A1:F463")

# Etapa 2: Preparação da tabela base ----------------------------------------
#Junção dos dados : mapeamento dos produtos alimentares e dos locais de aquisição
tb_aux_2018 <- tradutor_alimentacao_2018 %>% full_join(base_aquisicao_alimentar_2018, by = c('codigo_2018_trad' = 'V9001'))

tb_aux_2018 <-  tb_aux_2018 %>% full_join(tradutor_locais, by=c("V9004"="codigo_local"))

#Remove linhas sem dados 
tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

# Adiciona variável de contagem
tabela_aquisicao_2018 <- tb_aux_2018 %>%
  mutate(count = 1) %>%  
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC,class_analisegeral_final_bebidas,nome_local,cod_local,cnae_subclasse) %>%
  summarize(count = sum(count, na.rm = TRUE),
            valor_mensal = sum(valor_mensal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(key_morador = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC)) 


base_uc_2018 <- base_uc_2018 %>%
  mutate(across(c(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC), as.character)) %>%
  mutate(key_morador = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC))

# Juntando com a base morador_uc_key
tabela_final_2018 <- inner_join(tabela_aquisicao_2018, base_uc_2018, by = "key_morador")


# Etapa 3: Sumarização dos dados -----------------------------------------------
# Nível : Brasil ---------------------------------------------------------------
# Número de itens por classificação do decreto
tabfinal_br_nitens_decreto <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_analisegeral_final_bebidas = recode(class_analisegeral_final_bebidas,
                              "Leguminosa" = "leguminosa",
                              "Cereais" = "cereais",
                              "Tubérculos e raízes" = "raizes_tuber",
                              "Legumes e verduras" = "legumes_verduras",
                              "Frutas" = "frutas",
                              "Carnes e Ovos"="carnes_ovos",
                              "Leites e Queijos"= "leites_e_queijos",
                              "Farinha"= "farinha",
                              "Ultraprocessados"="ultra",
                              "Ultraprocessados_beb" = "ultra_beb",
                              "Castanhas e nozes" = "castanha_nozes",
                              "Café e Chá" = "cha_cafe",
                              "Outros" = "outros")) %>%
  group_by(class_analisegeral_final_bebidas) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))


br_nitens_decreto_local <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_analisegeral_final_bebidas = recode(class_analisegeral_final_bebidas,
                                                   "Leguminosa" = "leguminosa",
                                                   "Cereais" = "cereais",
                                                   "Tubérculos e raízes" = "raizes_tuber",
                                                   "Legumes e verduras" = "legumes_verduras",
                                                   "Frutas" = "frutas",
                                                   "Carnes e Ovos"="carnes_ovos",
                                                   "Leites e Queijos"= "leites_e_queijos",
                                                   "Farinha"= "farinha",
                                                   "Ultraprocessados"="ultra",
                                                   "Ultraprocessados_beb" = "ultra_beb",
                                                   "Castanhas e nozes" = "castanha_nozes",
                                                   "Café e Chá" = "cha_cafe",
                                                   "Outros" = "outros")) %>%
  group_by(class_analisegeral_final_bebidas,nome_local) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))


tabfinal_br_nitens_decreto_local <- br_nitens_decreto_local %>%
  # Transformar a tabela para formato wide
  pivot_wider(
    names_from = class_analisegeral_final_bebidas,  # Nome das colunas será o rótulo da classe
    values_from = total_2018,  # Valores preenchidos por total_2018
    values_fill = 0            # Preencher valores ausentes com 0
  ) %>%
  
  # Calcular o percentual de cada classe dentro do total comprado no local
  mutate(across(
    .cols = `carnes_ovos`:`ultra_beb`,   
    .fns =  ~ .x / sum(.x, na.rm = TRUE), 
    .names = "perc_{.col}"
  )) %>%
  
  # Reorganizar as colunas para deixar os percentuais próximos às classes
  relocate( 
    nome_local,
    leguminosa, perc_leguminosa,
    cereais, perc_cereais,
    raizes_tuber, perc_raizes_tuber,
    legumes_verduras,perc_legumes_verduras,
    frutas,perc_frutas,
    carnes_ovos, perc_carnes_ovos,
    leites_e_queijos, perc_leites_e_queijos, 
    farinha, perc_farinha, 
    ultra, perc_ultra,
    ultra_beb, perc_ultra_beb,
    castanha_nozes, perc_castanha_nozes,
    cha_cafe,perc_cha_cafe,
    outros,perc_outros
  ) %>%
  
  arrange(nome_local)

# Nível : Estados ---------------------------------------------------------------
#Gerando a tabela por estado
# Primeiro, mapeamos os códigos de UF para os nomes dos estados
uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
  "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
  "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "Es", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

es_nitens_decreto_local <- tabela_final_2018 %>%
  # Mapeando a coluna UF.x para o nome do estado com base no dicionário
  mutate(estado = recode(UF.x, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%  # Caso não encontre, atribui "Desconhecido"
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_analisegeral_final_bebidas = recode(class_analisegeral_final_bebidas,
                                                   "Leguminosa" = "leguminosa",
                                                   "Cereais" = "cereais",
                                                   "Tubérculos e raízes" = "raizes_tuber",
                                                   "Legumes e verduras" = "legumes_verduras",
                                                   "Frutas" = "frutas",
                                                   "Carnes e Ovos"="carnes_ovos",
                                                   "Leites e Queijos"= "leites_e_queijos",
                                                   "Farinha"= "farinha",
                                                   "Ultraprocessados"="ultra",
                                                   "Ultraprocessados_beb" = "ultra_beb",
                                                   "Castanhas e nozes" = "castanha_nozes",
                                                   "Café e Chá" = "cha_cafe",
                                                   "Outros" = "outros")) %>%
  group_by(estado, class_analisegeral_final_bebidas, nome_local) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE), .groups = "drop")

# Verificar a tabela gerada
head(es_nitens_decreto_local)
# Filtrando e ajustando a tabela
tabfinal_es_nitens_decreto_local <- es_nitens_decreto_local %>%
  
  # Filtrando para os estados especificados
  filter(estado %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "MT")) %>%
  
  # Transformar a tabela para formato wide
  pivot_wider(
    names_from = class_analisegeral_final_bebidas,  # Nome das colunas será o rótulo da classe
    values_from = total_2018,  # Valores preenchidos por total_2018
    values_fill = 0            # Preencher valores ausentes com 0
  ) %>%
  
  # Calcular o percentual de cada classe dentro do total comprado no local
  mutate(across(
    .cols = `carnes_ovos`:`ultra_beb`,   
    .fns =  ~ .x / sum(.x, na.rm = TRUE), 
    .names = "perc_{.col}"
  )) %>%
  
  # Reorganizar as colunas para deixar os percentuais próximos às classes
  relocate( 
    nome_local,
    leguminosa, perc_leguminosa,
    cereais, perc_cereais,
    raizes_tuber, perc_raizes_tuber,
    legumes_verduras,perc_legumes_verduras,
    frutas,perc_frutas,
    carnes_ovos, perc_carnes_ovos,
    leites_e_queijos, perc_leites_e_queijos, 
    farinha, perc_farinha, 
    ultra, perc_ultra,
    ultra_beb, perc_ultra_beb,
    castanha_nozes, perc_castanha_nozes,
    cha_cafe,perc_cha_cafe,
    outros,perc_outros
  ) %>%
  
  # Ordenar por cnae_subclasse e nome_local
  arrange(nome_local)
# Nível : Média estados -------------------------------------------------------
reamzl_nitens_decreto_local <- tabela_final_2018 %>%
  # Mapeando a coluna UF.x para o nome do estado com base no dicionário
  mutate(estado = recode(UF.x, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%  # Caso não encontre, atribui "Desconhecido"
  # Filtrando para os estados especificados
  filter(estado %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "MT")) %>%
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_analisegeral_final_bebidas = recode(class_analisegeral_final_bebidas,
                                                   "Leguminosa" = "leguminosa",
                                                   "Cereais" = "cereais",
                                                   "Tubérculos e raízes" = "raizes_tuber",
                                                   "Legumes e verduras" = "legumes_verduras",
                                                   "Frutas" = "frutas",
                                                   "Carnes e Ovos"="carnes_ovos",
                                                   "Leites e Queijos"= "leites_e_queijos",
                                                   "Farinha"= "farinha",
                                                   "Ultraprocessados"="ultra",
                                                   "Ultraprocessados_beb" = "ultra_beb",
                                                   "Castanhas e nozes" = "castanha_nozes",
                                                   "Café e Chá" = "cha_cafe",
                                                   "Outros" = "outros")) %>%
  group_by(class_analisegeral_final_bebidas, nome_local) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE), .groups = "drop")


tabfinal_reamzl_nitens_decreto_local <- reamzl_nitens_decreto_local %>%
  # Transformar a tabela para formato wide
  pivot_wider(
    names_from = class_analisegeral_final_bebidas,  # Nome das colunas será o rótulo da classe
    values_from = total_2018,  # Valores preenchidos por total_2018
    values_fill = 0            # Preencher valores ausentes com 0
  ) %>%
  
  # Calcular o percentual de cada classe dentro do total comprado no local
  mutate(across(
    .cols = `carnes_ovos`:`ultra_beb`,   
    .fns =  ~ .x / sum(.x, na.rm = TRUE), 
    .names = "perc_{.col}"
  )) %>%
  
  # Reorganizar as colunas para deixar os percentuais próximos às classes
  relocate( 
    nome_local,
    leguminosa, perc_leguminosa,
    cereais, perc_cereais,
    raizes_tuber, perc_raizes_tuber,
    legumes_verduras,perc_legumes_verduras,
    frutas,perc_frutas,
    carnes_ovos, perc_carnes_ovos,
    leites_e_queijos, perc_leites_e_queijos, 
    farinha, perc_farinha, 
    ultra, perc_ultra,
    ultra_beb, perc_ultra_beb,
    castanha_nozes, perc_castanha_nozes,
    cha_cafe,perc_cha_cafe,
    outros,perc_outros
  ) %>%
  
  # Ordenar por cnae_subclasse e nome_local
  arrange(nome_local)


# Etapa 4: Gerando tabela com os resultados finais ----------------------------
# Criar um arquivo Excel
wb <- createWorkbook()

# Adicionar a aba para a primeira tabela
addWorksheet(wb, "br_itens_classe_decreto")
writeData(wb, "br_itens_classe_decreto", tabfinal_br_nitens_decreto)

# Adicionar a aba para a segunda tabela
addWorksheet(wb, "br_itens_classe_decreto_local")
writeData(wb, "br_itens_classe_decreto_local", tabfinal_br_nitens_decreto_local)

# Identificar colunas que começam com "perc_"
cols_perc_br <- grep("^perc_", colnames(tabfinal_br_nitens_decreto_local))

# Criar estilo de célula verde
greenStyle <- createStyle(fontColour = "#FFFFFF", bgFill = "#4CAF50")

# Aplicar formatação condicional nas colunas de percentual
for (col in cols_perc_br) {
  # Agora, para as colunas perc_, vamos aplicar a formatação diretamente
  conditionalFormatting(
    wb, sheet = "br_itens_classe_decreto_local",
    cols = col, rows = 2:(nrow(tabfinal_br_nitens_decreto_local) + 1),
    rule = ">=0.5", style = greenStyle
  )
}


# Filtrar os dados por estado e adicionar cada estado como uma aba separada
estados <- unique(tabfinal_es_nitens_decreto_local$estado)

# Identificar colunas que começam com "perc_" na tabela original
cols_perc_original <- grep("^perc_", colnames(tabfinal_es_nitens_decreto_local))

for (estado_loop in estados) {
  # Filtra a tabela para o estado atual
  estado_data <- tabfinal_es_nitens_decreto_local %>% filter(estado == estado_loop)
  
  # Adiciona uma aba para o estado
  addWorksheet(wb, estado_loop)
  
  # Escreve os dados do estado na aba correspondente
  writeData(wb, sheet = estado_loop, x = estado_data)
  
  # Aplicar formatação condicional nas colunas de percentual (usando as colunas da tabela original)
  for (col in cols_perc_original) {
    conditionalFormatting(
      wb, sheet = estado_loop,
      cols = col, rows = 2:(nrow(estado_data) + 1),
      rule = ">=0.5", style = greenStyle
    )
  }
}


# Adicionar a aba para a segunda tabela
addWorksheet(wb, "reamzl_itens_decreto_local")
writeData(wb, "reamzl_itens_decreto_local", tabfinal_reamzl_nitens_decreto_local)

# Identificar colunas que começam com "perc_"
cols_perc_re <- grep("^perc_", colnames(tabfinal_reamzl_nitens_decreto_local))

# Criar estilo de célula verde
greenStyle <- createStyle(fontColour = "#FFFFFF", bgFill = "#4CAF50")

# Aplicar formatação condicional nas colunas de percentual
for (col in cols_perc_re) {
  # Agora, para as colunas perc_, vamos aplicar a formatação diretamente
  conditionalFormatting(
    wb, sheet = "reamzl_itens_decreto_local",
    cols = col, rows = 2:(nrow(tabfinal_reamzl_nitens_decreto_local) + 1),
    rule = ">=0.5", style = greenStyle
  )
}

# Salvar o arquivo
saveWorkbook(wb, file.path(outdir, "tabela_pof2018_decreto_locais_15marco2025.xlsx"), overwrite = TRUE)
