# ------------------------------------------------------------------------------
# Script para unir os resultados de Segurança alimentar de todos os anos (2004, 2009, 2013, 2018, 2023)
# ------------------------------------------------------------------------------
# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table,readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)  # Carrega o pacote
library(tidyr)
library(openxlsx)
library(ggplot2)

# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
intdir <-  paste(path, "data/intermediate/", sep = "/")
outdir <- paste(path, "data/outputs/", sep = "/")

# Etapa 1: Leitura dos dados ----------------------------------------------------
pnad_2004 <- read.csv(paste0(intdir,"tabela_seguranca_alimentar_pnad_2004_26marco2025.csv"),sep = ";")
pnad_2009 <- read.csv(paste0(intdir,"tabela_seguranca_alimentar_pnad_2009_26marco2025.csv"),sep = ";")
pnad_2013 <- read.csv(paste0(intdir,"tabela_seguranca_alimentar_pnad_2013_26marco2025.csv"),sep = ";")
pof_2018 <- read.csv(paste0(intdir,"tabela_seguranca_alimentar_pof_2018_26marco2025.csv"),sep = ";")
pnadc_2023 <- read.csv(paste0(intdir,"tabela_seguranca_alimentar_pnadc_2023_26marco2025.csv"),sep = ";")

# Adicionando a coluna 'fonte' em cada base de dados
pnad_2004 <- pnad_2004 %>% mutate(fonte = "PNAD 2004") %>% filter(seguranca_alimentar != "Não Aplicável")
pnad_2009 <- pnad_2009 %>% mutate(fonte = "PNAD 2009") %>% filter(seguranca_alimentar != "Não Aplicável")
pnad_2013 <- pnad_2013 %>% mutate(fonte = "PNAD 2013") %>% filter(seguranca_alimentar != "Não Aplicável")
pof_2018 <- pof_2018 %>% mutate(fonte = "POF 2018")
pnadc_2023 <- pnadc_2023 %>% mutate(fonte = "PNADC 2023")

# Unindo todas as bases
seg_alimentar_all <- bind_rows(pnad_2004, pnad_2009, pnad_2013, pof_2018, pnadc_2023)

# Seleciona os dados de interesse
seg_alimentar_all <- seg_alimentar_all %>% 
  filter(Regiao %in% c("Brasil","Rondônia","Acre","Amazonas",
                                                  "Roraima","Pará","Amapá","Tocantins",
                                                  "Maranhão","Mato Grosso")) %>% 
  select(fonte,Regiao,seguranca_alimentar,situacao_domicilio,num_domicilios)


# Adiciona Situação de Domicílio TOTAL (com sumarização entre Urbano e Rural)
# Criar linha de total por segurança alimentar
totais_situacao <- seg_alimentar_all %>%
  group_by(fonte, Regiao, seguranca_alimentar) %>%
  summarise(num_domicilios = sum(num_domicilios), .groups = "drop") %>%
  mutate(situacao_domicilio = "Total")

# Unir os totais ao conjunto original
seg_alimentar_all <- bind_rows(seg_alimentar_all, totais_situacao) %>%
  arrange(seguranca_alimentar, situacao_domicilio)

# Calculando os totais considerando todas as categorias de segurança alimentar,
# para calcular as porcentagens
totais <- seg_alimentar_all %>%
  group_by(fonte,Regiao,situacao_domicilio) %>%
  summarise(
    total_domicilios = sum(num_domicilios, na.rm = TRUE)
  )

# Unindo os totais com os dados principais
dados_completos <- seg_alimentar_all %>%
  left_join(totais, by = c('fonte','Regiao',"situacao_domicilio")) %>%
  mutate(
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

# Criando a linha de Insegurança Alimentar Total
inseguranca_total <- dados_completos %>%
  filter(grepl("Insegurança Alimentar", seguranca_alimentar)) %>%
  group_by(fonte,Regiao,situacao_domicilio) %>%
  summarise(
    seguranca_alimentar = "Insegurança Alimentar",
    num_domicilios = sum(num_domicilios),
    total_domicilios = first(total_domicilios)
  ) %>%
  mutate(
    percentual_domicilios = round((num_domicilios / total_domicilios) * 100, 1)
  )

dados_completos_final <- bind_rows(dados_completos, inseguranca_total)

write.table(seg_alimentar_all, paste(outdir,"tabela_seguranca_alimentar_numtotal_15marco2025.csv", sep = ""),row.names = F, sep = ";")
write.table(dados_completos_final, paste(outdir,"tabela_seguranca_alimentar__numtotal_perc_15marco2025.csv", sep = ""),row.names = F, sep = ";")


# ETAPA 3: EXPORTA TABELA FINAL
# Definir a ordem desejada para a coluna seguranca_alimentar
ordem_seg_alim <- c("Segurança Alimentar", "Insegurança Alimentar", 
                    "Insegurança Alimentar Leve", "Insegurança Alimentar Moderada", 
                    "Insegurança Alimentar Grave")

# Criar uma lista de regiões
regioes <- unique(dados_completos_final$Regiao)

# Criar um workbook do Excel
wb <- createWorkbook()

for (regiao in regioes) {
  # Filtrar os dados para a região específica
  dados_regiao <- dados_completos_final %>%
    filter(Regiao == regiao) %>%
    select(fonte, seguranca_alimentar, situacao_domicilio, percentual_domicilios) %>%
    pivot_wider(names_from = situacao_domicilio, values_from = percentual_domicilios, values_fill = 0) %>%
    mutate(seguranca_alimentar = factor(seguranca_alimentar, levels = ordem_seg_alim)) %>%
    arrange(seguranca_alimentar) %>%
    select(fonte, seguranca_alimentar, Total, Urbano, Rural)  # Ordenação correta das colunas
  
  # Renomear colunas para ficarem mais claras
  colnames(dados_regiao) <- c("Fonte", "Situação de Segurança Alimentar", "Total", "Urbano", "Rural")
  
  # Criar uma aba para a região no Excel
  addWorksheet(wb, regiao)
  
  # Escrever os dados na aba
  writeData(wb, regiao, dados_regiao)
}

# Salvar o arquivo Excel
saveWorkbook(wb,paste0(outdir,"tabela_seguranca_alimentar_domicilio_15marco2025.xlsx"), overwrite = TRUE)

cat("Arquivo Excel gerado com sucesso!\n")
