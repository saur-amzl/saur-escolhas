

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")
outdir <-  paste(path, "data/outputs", sep = "/")
inputdir <-  paste(path, "data/inputs", sep = "/")

source("consumo-de-alimentos/set_estrato.R")

# Etapa 1: Leitura dos dados ----------------------------------------------
#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_uc_2018 <- read.csv(paste0(pathdir,"tabela_base_uc_pof1718.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_prod_aquisicao_classes_v16fev25.xlsx"),sheet = '2018', range = "A1:AF4911")
tradutor_alimentacao_2018 <-tradutor_alimentacao_2018[c(2:4,22:25,30:32)]
colnames(tradutor_alimentacao_2018) <- c("codigo_2018_trad","produto_2018_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

tradutor_locais_2018_amzl <-
  readxl::read_excel(paste0(pathdir,"mapeamento_local_pof1718_amzl.xlsx"),sheet = 'Sheet1', range= "A1:E318")

tradutor_locais <-
  readxl::read_excel(paste0(inputdir,"/mapeamento_local_pof_rais_7marco2025.xlsx"),sheet = 'Sheet1', range= "A1:F463")

# Etapa 2: Preparação da tabela base ----------------------------------------
#Junção dos dados : mapeamento dos produtos alimentares e dos locais de aquisição
tb_aux_2018 <- tradutor_alimentacao_2018 %>% full_join(base_aquisicao_alimentar_2018, by = c('codigo_2018_trad' = 'V9001'))

tb_aux_2018 <-  tb_aux_2018 %>% full_join(tradutor_locais, by=c("V9004"="codigo_local"))

#Remove linhas sem dados 
tb_aux_2018 <- tb_aux_2018[!is.na(tb_aux_2018$valor_mensal), ] # [2]

# Adiciona variável de contagem
tabela_aquisicao_2018 <- tb_aux_2018 %>%
  mutate(count = 1) %>%  
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC,class_final,nome_local,cod_local,cnae_subclasse) %>%
  summarize(count = sum(count, na.rm = TRUE),
            valor_mensal = sum(valor_mensal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(key_morador = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC)) 


base_uc_2018 <- base_uc_2018 %>%
  mutate(across(c(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC), as.character)) %>%
  mutate(key_morador = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC))

# Juntando com a base morador_uc_key
tabela_final_2018 <- inner_join(tabela_aquisicao_2018, base_uc_2018, by = "key_morador")

colnames(tabela_final_2018)
# Etapa de validação
# Número de itens por classificação do GUIA
tabfinal_br_nitens_guia <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_final = recode(class_final,
                              "1" = "In_natura",
                              "4" = "Ultraprocessado",
                              "3" = "Processado",
                              "2" = "preparacao_culinaria",
                              "5" = "sem_classificacao")) %>%
  group_by(class_final) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))


br_nitens_guia_local <- tabela_final_2018 %>%
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_final = recode(class_final,
                              "1" = "In_natura",
                              "4" = "Ultraprocessado",
                              "3" = "Processado",
                              "2" = "preparacao_culinaria",
                              "5" = "sem_classificacao")) %>%
  group_by(class_final,nome_local,cnae_subclasse) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE))


tabfinal_br_nitens_guia_local <- br_nitens_guia_local %>%
  # Calcular o total de compras por nome_local e cnae_subclasse
  group_by(nome_local, cnae_subclasse) %>%
  mutate(total_compras = sum(total_2018, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Transformar a tabela para formato wide
  pivot_wider(
    names_from = class_final,  # Nome das colunas será o rótulo da classe
    values_from = total_2018,  # Valores preenchidos por total_2018
    values_fill = 0            # Preencher valores ausentes com 0
  ) %>%
  
  # Calcular o percentual de cada classe dentro do total comprado no local
  mutate(across(
    .cols = `In_natura`:`sem_classificacao`,   
    .fns = ~ (.x / total_compras), 
    .names = "perc_{.col}"
  )) %>%
  
  # Reorganizar as colunas para deixar os percentuais próximos às classes
  relocate( 
    nome_local, cnae_subclasse, total_compras,
    In_natura, perc_In_natura,
    Processado, perc_Processado,
    preparacao_culinaria, perc_preparacao_culinaria,
    Ultraprocessado, perc_Ultraprocessado,
    sem_classificacao, perc_sem_classificacao
  ) %>%
  
  # Ordenar por cnae_subclasse e nome_local
  arrange(cnae_subclasse, nome_local)

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

es_nitens_guia_local <- tabela_final_2018 %>%
  # Mapeando a coluna UF.x para o nome do estado com base no dicionário
  mutate(estado = recode(UF.x, 
                         !!!uf_map,      # Mapeamento de UF para nome do estado
                         .default = "Desconhecido")) %>%  # Caso não encontre, atribui "Desconhecido"
  mutate(itens = count * PESO_FINAL) %>%
  mutate(class_final = recode(class_final,
                              "1" = "In_natura",
                              "4" = "Ultraprocessado",
                              "3" = "Processado",
                              "2" = "preparacao_culinaria",
                              "5" = "sem_classificacao")) %>%
  group_by(estado, class_final, nome_local, cnae_subclasse) %>%
  summarise(total_2018 = sum(itens, na.rm = TRUE), .groups = "drop")

# Verificar a tabela gerada
head(es_nitens_guia_local)
# Filtrando e ajustando a tabela
tabfinal_es_nitens_guia_local <- es_nitens_guia_local %>%

    # Filtrando para os estados especificados
  filter(estado %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "MT")) %>%
  
  # Calcular o total de compras por nome_local e cnae_subclasse
  group_by(estado,nome_local, cnae_subclasse) %>%
  mutate(total_compras = sum(total_2018, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Transformar a tabela para formato wide
  pivot_wider(
    names_from = class_final,  # Nome das colunas será o rótulo da classe
    values_from = total_2018,  # Valores preenchidos por total_2018
    values_fill = 0            # Preencher valores ausentes com 0
  ) %>%
  
  # Calcular o percentual de cada classe dentro do total comprado no local
  mutate(across(
    .cols = `In_natura`:`sem_classificacao`,   
    .fns = ~ (.x / total_compras), 
    .names = "perc_{.col}"
  )) %>%
  
  # Reorganizar as colunas para deixar os percentuais próximos às classes
  relocate( 
    estado,nome_local, cnae_subclasse, total_compras,
    In_natura, perc_In_natura,
    Processado, perc_Processado,
    preparacao_culinaria, perc_preparacao_culinaria,
    Ultraprocessado, perc_Ultraprocessado,
    sem_classificacao, perc_sem_classificacao
  ) %>%
  
  # Ordenar por cnae_subclasse e nome_local
  arrange(cnae_subclasse, nome_local)


# Gerando tabela com os resultados 
# Carregar o pacote
library(openxlsx)

# Criar um arquivo Excel
wb <- createWorkbook()

# Adicionar a aba para a primeira tabela
addWorksheet(wb, "br_itens_classe_guia")
writeData(wb, "br_itens_classe_guia", tabfinal_br_nitens_guia)

# Adicionar a aba para a segunda tabela
addWorksheet(wb, "br_itens_classe_guia_local")
writeData(wb, "br_itens_classe_guia_local", tabfinal_br_nitens_guia_local)

# Identificar colunas que começam com "perc_"
cols_perc_br <- grep("^perc_", colnames(tabfinal_br_nitens_guia_local))

# Criar estilo de célula verde
greenStyle <- createStyle(fontColour = "#FFFFFF", bgFill = "#4CAF50")

# Aplicar formatação condicional nas colunas de percentual
for (col in cols_perc_br) {
  # Agora, para as colunas perc_, vamos aplicar a formatação diretamente
  conditionalFormatting(
    wb, sheet = "br_itens_classe_guia_local",
    cols = col, rows = 2:(nrow(tabfinal_br_nitens_guia_local) + 1),
    rule = ">=0.5", style = greenStyle
  )
}


# Filtrar os dados por estado e adicionar cada estado como uma aba separada
estados <- unique(tabfinal_es_nitens_guia_local$estado)

# Identificar colunas que começam com "perc_" na tabela original
cols_perc_original <- grep("^perc_", colnames(tabfinal_es_nitens_guia_local))

for (estado_loop in estados) {
  # Filtra a tabela para o estado atual
  estado_data <- tabfinal_es_nitens_guia_local %>% filter(estado == estado_loop)
  
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

# Salvar o arquivo
saveWorkbook(wb, file.path(outdir, "tab_pof2018_guia_locais2.xlsx"), overwrite = TRUE)
