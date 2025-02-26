
## Script original disponibilizado juntamente com os microdados da POF 2007-2008
## Scripts 'Tabela de Alimentacao.R

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")


# Tabela de Alimentação ---------------------------------------------------
#  Leitura do REGISTRO - CADERNETA COLETIVA (Questionario POF 3)
caderneta_coletiva <- readRDS(paste0(pathdir,"2007-2008/Dados_20231009/CADERNETA_COLETIVA.rds"))

# [1] Transformacao do codigo do item (variavel V9001) em 5 numeros, para ficar no mesmo padrao dos codigos que constam nos arquivos de
#     tradutores das tabelas. Esses c?digos s?o simplificados em 5 n?meros, pois os 2 ?ltimos n?meros caracterizam sin?nimos ou termos 
#     regionais do produto. Todos os resultados da pesquisa s?o trabalhados com os c?digos considerando os 5 primeiros n?meros.
#     Por exemplo, tangerina e mexirica tem c?digos diferentes quando se considera 7 n?meros, por?m o mesmo c?digo quando se considera os 
#     5 primeiros n?meros. 

# [2] Exclus?o dos itens do REGISTRO - CADERNETA COLETIVA (POF 3) que n?o se referem a alimentos (grupos 86 a 89, ver cadastro de produtos). 

# [3] Anualiza??o e expans?o dos valores utilizados para a obten??o dos resultados (vari?vel V8000_defla). 
#     a) Para anualizar, utilizamos o quesito "fator_anualizacao". Os valores s?o anualizados para depois se obter uma m?dia mensal.
#     b) Para expandir, utilizamos o quesito "peso_final".
#     c) Posteriormente, o resultado ? dividido por 12 para obter a estimativa mensal.

cad_coletiva <- 
  transform( subset( transform( caderneta_coletiva ,
                                codigo = trunc(V9001/100) # [1]
  ),
  codigo < 86001 | codigo > 89999
  ),
  valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12,# [3] 
  qtidade_anual = (QTD_FINAL* FATOR_ANUALIZACAO*PESO_FINAL)
  )
rm(caderneta_coletiva)

# Leitura do REGISTRO - DESPESA INDIVIDUAL (Question?rio POF 4)
despesa_individual <- readRDS(paste0(pathdir,"2007-2008/Dados_20231009/DESPESA_INDIVIDUAL.rds"))

# [1] Transforma??o do c?digo do item (vari?vel V9001) em 5 n?meros, para ficar no mesmo padr?o dos c?digos que constam nos arquivos de
#     tradutores das tabelas. Esses c?digos s?o simplificados em 5 n?meros, pois os 2 ?ltimos n?meros caracterizam sin?nimos ou termos 
#     regionais do produto. Todos os resultados da pesquisa s?o trabalhados com os c?digos considerando os 5 primeiros n?meros.

# [2] Sele??o dos itens do REGISTRO - DESPESA INDIVIDUAL (POF 4) que entram na tabela de alimenta??o 
#     (todos do quadro 24 e c?digos 41006,48033,49026).   

# [3] Anualiza??o e expans?o dos valores utilizados para a obten??o dos resultados (vari?vel V8000_defla). 
#     a) Para anualizar, utilizamos o quesito "fator_anualizacao". No caso espec?fico dos quadros 48 e 49,
#        cujas informa??es se referem a valores mensais, utilizamos tamb?m o quesito V9011 (n?mero de meses).
#        Os valores s?o anualizados para depois se obter uma m?dia mensal.

#     b) Para expandir, utilizamos o quesito "peso_final".
#     c) Posteriormente, o resultado ? dividido por 12 para obter a estimativa mensal. 
colnames(despesa_individual)
desp_individual <- 
  subset( transform( despesa_individual,
                     codigo = trunc(V9001/100) # [1]
  ),
  NUM_QUADRO==24|codigo==41006|codigo==48033|codigo==49026
  ) # [2]

desp_individual <-
  transform( desp_individual,
             valor_mensal =  (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12) # [3] 
  
rm(despesa_individual)

# [1] Jun??o dos registros CADERNETA COLETIVA e DESPESA INDIVIDUAL, quem englobam os itens de alimenta??o. 

# As duas tabelas precisam ter o mesmo conjunto de vari?veis
# Identifica??o dos nomes das vari?veis das tabelas a serem juntadas:
nomes_cad <- names(cad_coletiva)
nomes_desp <- names(desp_individual)

# Identifica??o das vari?veis exclusivas a serem inclu?das na outra tabela:
incl_cad <- nomes_desp[!nomes_desp %in% nomes_cad]
incl_desp <- nomes_cad[!nomes_cad %in% nomes_desp]

# Criando uma tabela com NAs das vari?veis ausentes em cada tabela
col_ad_cad <- data.frame(matrix(NA,
                                nrow(cad_coletiva),
                                length(incl_cad)))
names(col_ad_cad) <- incl_cad
col_ad_desp <- data.frame(matrix(NA,
                                 nrow(desp_individual),
                                 length(incl_desp)))
names(col_ad_desp) <- incl_desp

# Acrescentando as colunas ausentes em cada tabela:
cad_coletiva <- cbind(cad_coletiva ,
                      col_ad_cad)
desp_individual <- cbind( desp_individual , 
                          col_ad_desp)

# Juntando (empilhando) as tabelas com conjuntos de vari?veis iguais
junta_ali <- 
  rbind( cad_coletiva , desp_individual ) # [1]



# # Tabela UC e Individuo -------------------------------------------------

# Leitura do REGISTRO - MORADOR, necess?rio para o c?lculo do n?mero de UC's expandido.
# Vale ressaltar que este ? o ?nico registro dos microdados que engloba todas as UC's

# Extraindo todas as UC's do arquivo de morador
morador_uc <- 
  unique( 
    readRDS( 
      paste0(pathdir,"2007-2008/Dados_20231009/MORADOR.rds")
    ) [ ,
        c("COD_UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
            "PESO_FINAL"
        ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC


morador_pessoas <- 
  unique( 
    readRDS( 
      paste0(pathdir,"2007-2008/Dados_20231009/MORADOR.rds")
    ) [ ,
        c("COD_UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE",
          "PESO_FINAL"
        ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC



# Salva o dado 
write.table(junta_ali, paste(pathdir,"tabela_base_alimentacao_pof0708.csv", sep = ""),
            row.names = F, sep = ";")

write.table(morador_uc, paste(pathdir,"tabela_base_uc_pof0708.csv", sep = ""),
            row.names = F, sep = ";")

write.table(morador_pessoas, paste(pathdir,"tabela_base_pessoas_pof0708.csv", sep = ""),
            row.names = F, sep = ";")
