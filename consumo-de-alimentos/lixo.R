install.packages("haven")


library(haven)
library(dplyr)

# Replace 'your_file.dta' with the path to your file
data <- read_dta("/Users/marlucescarabello/Downloads/Processamentos P2 Giovani/POF/dic_alimentos_gpp.dta")

data2 <- read_dta('/Users/marlucescarabello/Downloads/Processamentos P2 Giovani/POF/dic_alimentos.dta')
compras_pof <- read_dta('/Users/marlucescarabello/Downloads/Processamentos P2 Giovani/POF/pof_compras.dta')


confere <- data2 %>% group_by(classe_prod) %>% summarise(n=n())



locais_pof <- read_dta('/Users/marlucescarabello/Downloads/Processamentos P2 Giovani/POF/dic_locais.dta')
locais_oripof <- read_dta('/Users/marlucescarabello/Downloads/Processamentos P2 Giovani/POF/dic_local_origpof.dta')

locais_pof$v9004 <- as.integer(locais_pof$v9004)
locais <- tradutor_locais_2018_clean %>% 
  left_join(locais_pof,by = c("codigo_local"="v9004"))

write.csv2(data2,"tabela_data2.csv",sep = ";")


write.table(data2, paste("tabela_data2.csv", sep = ""),row.names = F, sep = ";")

write.table(locais, paste("tradutor_locais_aqui_pof1718.csv", sep = ""),row.names = F, sep = ";")

mapeamento_pof2018 <- read.csv("/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/produtos_alimentos_prof2018.csv",sep=",")
colnames(mapeamento_pof2018)

mapeamento_pof2008 <- read.csv("/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-amzl/data/produtos_alimentos_pof2008.csv",sep=",")
colnames(mapeamento_pof2008)

##compara as duas bases
match_pof <- mapeamento_pof2018 %>%
  inner_join(mapeamento_pof2008, by = c("descricao_produto" = "descricao_produto_08", "codigo_2018" = "codigo_2008"))

#identifica os códigos que deram matcj
cod_iguais_2008 <- match_pof %>% pull(codigo_2008_v2)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2008 <- mapeamento_pof2008 %>%
  filter(!(codigo_2008 %in% cod_iguais_2008))
  
#identifica os códigos que deram matcj
cod_iguais_2018 <- match_pof %>% pull(codigo_2018)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2018 <- mapeamento_pof2018 %>%
  filter(!(codigo_2018 %in% cod_iguais_2018))

##compara as duas bases
match_pof_descr <- pofdiferentes_2018 %>%
  inner_join(pofdiferentes_2008, by = c("descricao_produto" = "descricao_produto_08"))

#identifica os códigos que deram matcj
cod_iguais_2008v2 <- match_pof_descr %>% pull(codigo_2008_v2)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2008_v2 <- pofdiferentes_2008 %>%
  filter(!(codigo_2008 %in% cod_iguais_2008v2))

#identifica os códigos que deram matcj
cod_iguais_2018v2 <- match_pof_descr %>% pull(codigo_2018)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2018_v2 <- pofdiferentes_2018 %>%
  filter(!(codigo_2018 %in% cod_iguais_2018v2))


##compara as duas bases
match_pof_cod <- pofdiferentes_2018_v2 %>%
  inner_join(pofdiferentes_2008_v2, by = c("codigo_2018" = "codigo_2008"))

#identifica os códigos que deram matcj
cod_iguais_2008v2 <- match_pof_descr %>% pull(codigo_2008_v2)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2008_v2 <- pofdiferentes_2008 %>%
  filter(!(codigo_2008 %in% cod_iguais_2008v2))

#identifica os códigos que deram matcj
cod_iguais_2018v2 <- match_pof_descr %>% pull(codigo_2018)  # Supondo que a coluna seja 'codigo_2008'

pofdiferentes_2018_v2 <- pofdiferentes_2018 %>%
  filter(!(codigo_2018 %in% cod_iguais_2018v2))


write.table(pofdiferentes_2008_v2, paste("tabela_nomatch_2008.csv", sep = ""),row.names = F, sep = ";")
write.table(pofdiferentes_2018_v2, paste("tabela_nomatch_2018.csv", sep = ""),row.names = F, sep = ";")




colnames(match_pof)
nao_batem <- match_pof %>%
  filter(is.na(codigo_2018) | is.na(cod08))


write.table(match_pof, paste("tabela_match_pof.csv", sep = ""),row.names = F, sep = ";")




match <- mapeamento_pof2018 %>%
  full_join(data2,by=c("codigo_2018"="v9001"))

write.table(match, paste("mapeamento_pof2018_nova.csv", sep = ""),row.names = F, sep = ";")


match2 <- match %>%
  full_join(mapeamento_pof2008,by=c("codigo_2018"="codigo_2008"))

