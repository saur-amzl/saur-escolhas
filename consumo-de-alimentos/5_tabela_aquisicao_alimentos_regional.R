
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")

source("consumo-de-alimentos/set_estrato.R")
source("consumo-de-alimentos/processar_aquisicao.R")

# Etapa 1: Leitura dos dados ----------------------------------------------
#2007-2008
base_aquisicao_alimentar_2008 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof0708.csv"),sep = ";")
base_pessoas_2008 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof0708.csv"),sep = ";")

base_pessoas_2008 <- base_pessoas_2008 %>% rename(UF = COD_UF) 

#2017-2018
base_aquisicao_alimentar_2018 <- read.csv(paste0(pathdir,"tabela_base_alimentacao_pof1718.csv"),sep = ";")
base_pessoas_2018 <- read.csv(paste0(pathdir,"tabela_base_pessoas_pof1718.csv"),sep = ";")

#Tradutor - Aquisicao de alimentos
tradutor_alimentacao_2008 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_prod_aquisicao_classes_v16fev25.xlsx"),sheet = '2008', range = "A1:AC5136")
tradutor_alimentacao_2008 <-tradutor_alimentacao_2008[c(4:6,19:22,27:29)]
colnames(tradutor_alimentacao_2008) <- c("codigo_2008_trad","produto_2008_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

tradutor_alimentacao_2018 <-
  readxl::read_excel(paste0(pathdir,"mapeamento_prod_aquisicao_classes_v16fev25.xlsx"),sheet = '2018', range = "A1:AF4911")
tradutor_alimentacao_2018 <-tradutor_alimentacao_2018[c(2:4,22:25,30:32)]
colnames(tradutor_alimentacao_2018) <- c("codigo_2018_trad","produto_2018_trad","codigo_trad","is_regional","regiao","grupo_regional","item_regional","class_final","class_analisegeral_final","class_analisegeral_final_bebidas")

#-- 2008
#Junção dos dados
tb_aux_2008 <- base_aquisicao_alimentar_2008 %>%
  inner_join(tradutor_alimentacao_2008, by = c("V9001" = "codigo_2008_trad")) %>%
  rename(UF = COD_UF) %>%
  filter(!is.na(qtidade_anual))


#-- 2018
#Junção dos dados
tb_aux_2018 <- base_aquisicao_alimentar_2018 %>%
  inner_join(tradutor_alimentacao_2018, by = c("V9001" = "codigo_2018_trad")) %>%
  filter(!is.na(qtidade_anual))


tab_estado_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-UF$", agregar_por_estrato = TRUE)
tab_estado_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-UF$", agregar_por_estrato = TRUE)

tab_estado <- full_join(
  tab_estado_2008 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_estado_2018 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao_estrato","descricao","indicador_nivel")
) %>% 
  mutate(nivel = 'Estado') %>%
  select(nivel,everything())

tab_capital_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-UCapital$", agregar_por_estrato = TRUE)
tab_capital_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-UCapital$", agregar_por_estrato = TRUE)

tab_capital <- full_join(
  tab_capital_2008 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_capital_2018 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao_estrato","descricao","indicador_nivel")
) %>% 
  mutate(nivel = 'Capital') %>%
  select(nivel,everything())


tab_rm_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-CapitalRRM$", agregar_por_estrato = TRUE)
tab_rm_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-CapitalRRM$", agregar_por_estrato = TRUE)


tab_rm <- full_join(
  tab_rm_2008 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_rm_2018 %>% select(descricao_estrato,descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao_estrato","descricao","indicador_nivel")
) %>% 
mutate(nivel = 'RM') %>%
  select(nivel,everything())


tab_media_estado_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-UF$", agregar_por_estrato = FALSE)
tab_media_estado_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-UF$", agregar_por_estrato = FALSE)

tab_mediaestado <- full_join(
  tab_media_estado_2008 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_media_estado_2018 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao","indicador_nivel")
) %>%
  mutate(nivel = 'Média-UF',
         descricao_estrato = 'Média - Estados AMZL') %>%
  select(nivel,descricao_estrato, everything())

tab_media_capital_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-UCapital$", agregar_por_estrato = FALSE)
tab_media_capital_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-UCapital$", agregar_por_estrato = FALSE)

tab_mediacapital <- full_join(
  tab_media_capital_2008 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_media_capital_2018 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao","indicador_nivel")
) %>%
  mutate(nivel = 'Média-Capital',
         descricao_estrato = 'Média - Capitais AMZL') %>%
  select(nivel,descricao_estrato, everything())

tab_media_rm_2018 <- processar_aquisicao(tb_aux_2018, tabela_estratos_2018, base_pessoas_2018, "-CapitalRRM$", agregar_por_estrato = FALSE)
tab_media_rm_2008 <- processar_aquisicao(tb_aux_2008, tabela_estratos_2008, base_pessoas_2008, "-CapitalRRM$", agregar_por_estrato = FALSE)

tab_mediarm <- full_join(
  tab_media_rm_2008 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2008 = qtd_anual_percapita),
  tab_media_rm_2018 %>% select(descricao, indicador_nivel, qtd_anual_percapita) %>% rename(qtd_anual_percapita_2018 = qtd_anual_percapita),
  by = c("descricao","indicador_nivel")
) %>%
  mutate(nivel = 'Média-RM',
         descricao_estrato = 'Média - RM AMZL') %>%
  select(nivel,descricao_estrato, everything())

tab_combinada <- rbind(tab_estado, tab_capital, tab_rm, tab_mediaestado, tab_mediacapital, tab_mediarm)

write.table(tab_combinada, paste(pathdir,"tabela_aquisicao_2008_2018_regional_16fev2025.csv", sep = ""),row.names = F, sep = ";")


library(openxlsx)

# Crie um workbook
wb <- createWorkbook()

# 1. Adicione a primeira aba com a tabela completa
addWorksheet(wb, "Completa")
writeData(wb, sheet = "Completa", x = tab_combinada)

# 2. Para cada nível, crie uma aba e organize descricao_estrato em blocos de colunas
niveis <- unique(tab_combinada$nivel)

for(i in niveis) {
  # Filtra os dados apenas para o nível específico
  dados_nivel <- subset(tab_combinada, nivel == i)
  
  # Nome da aba (remova espaços e caracteres especiais)
  nome_aba <- gsub("[^[:alnum:]_]", "", i)
  addWorksheet(wb, nome_aba)
  
  # Identifica todos os descricao_estrato desse nível
  estratos <- unique(dados_nivel$descricao_estrato)
  
  # Controle de colunas (inicia na primeira coluna)
  col_start <- 1
  
  for(estrato in estratos) {
    # Filtra o estrato atual
    dados_estrato <- subset(dados_nivel, descricao_estrato == estrato)
    
    # Remove a coluna descricao_estrato, pois será o título do bloco
    dados_estrato$descricao_estrato <- NULL
    
    # Escreve o título do estrato na primeira linha do bloco
    writeData(wb, sheet = nome_aba, x = data.frame(estrato), startRow = 1, startCol = col_start, colNames = FALSE)
    
    # Escreve os dados do estrato abaixo do título
    writeData(wb, sheet = nome_aba, x = dados_estrato, startRow = 2, startCol = col_start)
    
    # Calcula a próxima coluna de início (fim do bloco atual + 2 colunas de espaço)
    col_start <- col_start + ncol(dados_estrato) + 2
  }
}

# Salve o arquivo Excel
saveWorkbook(wb, file = "tab_aquisicao_qtidade_niveis_16fev2025.xlsx", overwrite = TRUE)

