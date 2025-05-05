## Script original disponibilizado juntamente com os microdados da POF 2008-2009

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, tidyr,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/raw/", sep = "/")

# Etapa 1: Transformar os dados .txr em rds. Script original dispo --------
# nível na pasta Leitura dos dados da POF
# REGISTRO: DOMICÕLIO - POF1 / QUADRO 2 (TIPO_REG=01)
DOMICILIO <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_DOMICILIO_S.txt") 
           , widths = c(2,2,3,1,2,2,14,14,4,4,2,2,2,2,2,2,2,2,2,
                        2,2,2,2,2,2,2,1,1,1,16,16,16,1,1,1,1,1,1,
                        1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,6,1
           )
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","ESTRATO_POF","PESO",
                           "PESO_FINAL","PERIODO_REAL","QTD_MORADOR_DOMC","QTD_UC",             
                           "QTD_FAMILIA","V0202","V0203","V0204","V0205","V0206","V0207",
                           "V0208","V0209","V0210","V0211","V0219","V0221","V0222",
                           "V0223","IMPUT_QTD_COMODOS","IMPUT_QTD_BANHEIROS","IMPUT_ESGOTO",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL","V0224",
                           "V02011","V02012","V02013","V02014","V02015","V02016","V02017",
                           "V02018","V02019","V0212","V0213","V0214","V02151","V02152",
                           "V02161","V02162","V02163","V02164","V02165","V02166","V02167",
                           "V02171","V02172","V02173","V02174","V02175","V02181","V02182",             
                           "V02183","V02184","V02185","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DOMICILIO,paste0(pathdir,"2008-2009/Dados_20231009/","DOMICILIO.rds"))
rm(DOMICILIO)

# REGISTRO: PESSOAS - POF1 / QUADROS 3 E 4 (TIPO_REG = 02) */
MORADOR <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_MORADOR_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,2,2,2,2,2,
                        4,3,6,7,2,2,2,2,2,2,2,2,2,2,2,2,2,
                        2,2,2,2,2,16,16,16,2,5,5,5,5,5,5,5,
                        16,8,2,2,2,2,2,2,2,2,2,2,2,2,2,6,1,
                        1,1,3,5)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "COND_UNIDADE_CONSUMO","NUM_FAMILIA","COND_FAMILIA",
                           "V0401","V04041","V04042","V04043","IDADE_ANOS","IDADE_MES",
                           "IDADE_DIA","V0405","V0418","V0419","V0420","V0421","V0422",
                           "V0425","V0426","V0427","V0428","ANOS_ESTUDO","V0429","V0441",
                           "V0442","V0443","V0444","V0445","V0446","RENDA_MONETARIA",
                           "RENDA_NAO_MONETARIA","RENDA_TOTAL","V0406","V0433",
                           "V0434","V0436","V0437","COMPRIMENTO_IMPUTADO","ALTURA_IMPUTADA",
                           "PESO_IMPUTADO","RENDA_PERCAPITA","V04301","V0438","V0439",
                           "V0440","V0447","TEVE_NECESSIDADE_MEDICAMENTO",
                           "PRECISOU_ALGUM_SERVICO","V0407","V0408","V0415","V0416",
                           "V0417","V0423","V0424","COD_UPA","TIPO_SITUACAO_REG",
                           "NIVEL_INSTRUCAO_MORADOR","NIVEL_INSTRUCAO_PESS_REF",
                           "ALTURA_REFERIDA", "PESO_REFERIDO")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR,paste0(pathdir,"2008-2009/Dados_20231009/","MORADOR.rds"))
rm(MORADOR)

# REGISTRO: PESSOAS - IMPUTA«√O - POF1 / QUADRO 4 (TIPO_REG = 03)
MORADOR_IMPUT <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_MORADOR_IMPUT_S.txt") 
           , widths = c(2,2,3,1,2,1,2,2,14,14,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,16,16,16,1,1,1,1,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "COD_SABE_LER","COD_FREQ_ESCOLA","COD_CURSO_FREQ",
                           "COD_DUR_PRIMEIRO_GRAU_EH","COD_SERIE_FREQ",
                           "COD_NIVEL_INSTR","COD_DUR_PRIMEIRO_GRAU_ERA",
                           "COD_SERIE_COM_APROVACAO","COD_CONCLUIU_CURSO",
                           "COD_TEM_CARTAO","COD_EHTITULAR_CARTAO","COD_TEM_CHEQUE",
                           "COD_EHTITULAR_CONTA","RENDA_MONETARIA","RENDA_NAO_MONETARIA",
                           "RENDA_TOTAL","COD_TEM_PLANO","COD_EHTITULAR",
                           "COD_NUM_DEPENDENTE","COD_LEITE_MATERNO","COD_MESES_LEITE_MATERNO")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR_IMPUT,paste0(pathdir,"2008-2009/Dados_20231009/","MORADOR_IMPUT.rds"))
rm(MORADOR_IMPUT)

# REGISTRO: CONDI«’ES DE VIDA - POF6 (TIPO_REG = 04)
CONDICOES_VIDA <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_CONDICOES_DE_VIDA_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,1,1,1,16,16,16,11,11,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "V6101","V6104","V6105","RENDA_MONETARIA","RENDA_NAO_MONETARIA",
                           "RENDA_TOTAL","V6102","V6103","V6106","V61071","V61072",
                           "V61073","V610710","V61074","V61075","V61076","V61077","V61078",
                           "V61079","V610711","V61081","V61082","V61083","V61084","V61085",		        
                           "V61086","V61087","V61088","V61089","V6109","V61101","V61102", 		    
                           "V61103","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CONDICOES_VIDA,paste0(pathdir,"2008-2009/Dados_20231009/","CONDICOES_VIDA.rds"))
rm(CONDICOES_VIDA)

# REGISTRO: INVENT¡RIO DE BENS DUR¡VEIS - POF2 / QUADRO 14 (TIPO_REG = 05)
INVENTARIO <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_INVENTARIO_S.txt") 
           , widths = c(2,2,3,1,2,1,2,14,14,2,5,2,4,1,
                        2,2,16,16,16,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM",
                           "NUM_UC","ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO",
                           "COD_ITEM","V9005","V1404","V9012","V9002","COD_IMPUT",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL",
                           "V9001","SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(INVENTARIO,paste0(pathdir,"2008-2009/Dados_20231009/","INVENTARIO.rds"))
rm(INVENTARIO)

# REGISTRO: DESPESA DE 90 DIAS - POF2 / QUADROS 6 A 9 (TIPO_REG = 06)
DESPESA_90DIAS <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_DESPESA_90DIAS_S.txt")
           , widths = c( 2,2,3,1,2,1,2,14,14,2,5,2,11,2,5,11,16,
                         2,16,16,16,4,5,5,14,2,5,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","COD_ITEM",
                           "V9002","V8000","FATOR_ANUALIZACAO","DEFLATOR","V8000_DEFLA",
                           "VALOR_ANUAL_EXPANDIDO","COD_IMPUT","RENDA_MONETARIA",
                           "RENDA_NAO_MONETARIA","RENDA_TOTAL","V9005","V9007","V9009",                
                           "QTD_FINAL","COD_IMPUT_QUANTIDADE","V9004","V9001",
                           "SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_90DIAS,paste0(pathdir,"2008-2009/Dados_20231009/","DESPESA_90DIAS.rds"))
rm(DESPESA_90DIAS)

# REGISTRO: DESPESA DE 12 MESES - POF2 / QUADROS 10 A 13 (TIPO_REG = 07)
DESPESA_12MESES <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_DESPESA_12MESES_S.txt")
           , widths = c( 2,2,3,1,2,1,2,14,14,2,5,2,11,2,2,2,5,
                         11,16,2,16,16,16,5,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","COD_ITEM",
                           "V9002","V8000","V9010","V9011","FATOR_ANUALIZACAO","DEFLATOR",
                           "V8000_DEFLA","VALOR_ANUAL_EXPANDIDO","COD_IMPUT",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL",
                           "V9004","V9001","SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_12MESES,paste0(pathdir,"2008-2009/Dados_20231009/","DESPESA_12MESES.rds"))
rm(DESPESA_12MESES)

# REGISTRO: OUTRAS DESPESAS - POF2 / QUADROS 15 A 18 (TIPO_REG = 08)
OUTRAS_DESPESAS <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_OUTRAS_DESPESAS_S.txt")
           , widths = c( 2,2,3,1,2,1,2,14,14,2,5,2,11,1,2,5,
                         11,16,2,16,16,16,5,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","COD_ITEM",
                           "V9002","V8000","V9012","FATOR_ANUALIZACAO","DEFLATOR",
                           "V8000_DEFLA","VALOR_ANUAL_EXPANDIDO","COD_IMPUT",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL",
                           "V9004","V9001","SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(OUTRAS_DESPESAS,paste0(pathdir,"2008-2009/Dados_20231009/","OUTRAS_DESPESAS.rds"))
rm(OUTRAS_DESPESAS)

# REGISTRO: DESPESA COM SERVI«OS DOM…STICOS - POF2 / QUADRO 19 (TIPO_REG = 09)
SERVICO_DOMS <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_SERVICO_DOMS_S.txt") 
           , widths = c(2,2,3,1,2,1,2,14,14,2,5,2,11,5,11,
                        1,2,2,2,5,11,11,16,16,2,2,16,16,16,
                        7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","COD_ITEM",
                           "V9002","V8000","COD_INSS","V1904","V1905","V9010","V9011",
                           "FATOR_ANUALIZACAO","DEFLATOR","V8000_DEFLA","V1904_DEFLA",
                           "VALOR_ANUAL_EXPANDIDO","VALOR_INSS_ANUAL_EXPANDIDO",
                           "COD_IMPUT","COD_IMPUT_INSS","RENDA_MONETARIA",
                           "RENDA_NAO_MONETARIA","RENDA_TOTAL","V9001",
                           "SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(SERVICO_DOMS,paste0(pathdir,"2008-2009/Dados_20231009/","SERVICO_DOMS.rds"))
rm(SERVICO_DOMS)


# REGISTRO: ALUGUEL ESTIMADO - POF1 / QUADRO 2 (TIPO_REG = 10)
ALUGUEL_ESTIMADO <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_ALUGUEL_ESTIMADO_S.txt")
           , widths = c(2,2,3,1,2,1,2,14,14,2,5,2,11,
                        2,2,2,5,11,16,2,16,16,16,7,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","COD_ITEM",
                           "V9002","V8000","V9010","V9011","FATOR_ANUALIZACAO","DEFLATOR",
                           "V8000_DEFLA","VALOR_ANUAL_EXPANDIDO","COD_IMPUT",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL",
                           "V9001","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(ALUGUEL_ESTIMADO,paste0(pathdir,"2008-2009/Dados_20231009/","ALUGUEL_ESTIMADO.rds"))
rm(ALUGUEL_ESTIMADO)

# REGISTRO: CADERNETA DE DESPESA - POF3 (TIPO_REG = 11)
CADERNETA_COLETIVA <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_CADERNETA_DESPESA_S.txt") 
           , widths = c(2,2,3,1,2,1,2,14,14,2,2,5,2,11,2,5,
                        11,16,2,16,16,16,2,8,5,10,5,5,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "ESTRATO_POF","PESO","PESO_FINAL","NUM_QUADRO","NUM_GRUPO",
                           "COD_ITEM","V9002","V8000","FATOR_ANUALIZACAO","DEFLATOR",
                           "V8000_DEFLA","VALOR_ANUAL_EXPANDIDO","COD_IMPUT",
                           "RENDA_MONETARIA","RENDA_NAO_MONETARIA","RENDA_TOTAL",
                           "METODO_QUANTIDADE","QTD_FINAL","V9004","V9005","V9007",
                           "V9009","V9001","SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CADERNETA_COLETIVA,paste0(pathdir,"2008-2009/Dados_20231009/","CADERNETA_COLETIVA.rds"))
rm(CADERNETA_COLETIVA)

# REGISTRO: DESPESA INDIVIDUAL - POF4 / QUADROS 22 A 50 (TIPO_REG = 12)
DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_DESPESA_INDIVIDUAL_S.txt") 
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,5,2,11,2,5,11,16,
                        2,16,16,16,2,5,2,2,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "NUM_QUADRO","COD_ITEM","V9002","V8000","FATOR_ANUALIZACAO",
                           "DEFLATOR","V8000_DEFLA","VALOR_ANUAL_EXPANDIDO",
                           "COD_IMPUT","RENDA_MONETARIA","RENDA_NAO_MONETARIA",
                           "RENDA_TOTAL","V2905","V9004","V4104","V4105","V9001",
                           "SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_INDIVIDUAL,paste0(pathdir,"2008-2009/Dados_20231009/","DESPESA_INDIVIDUAL.rds"))
rm(DESPESA_INDIVIDUAL)

# REGISTROS: DESPESA COM VEÕCULOS - POF4 / QUADRO 51 (TIPO_REG = 13)
DESPESA_VEICULO <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_DESPESA_VEICULO_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,5,2,11,
                        1,2,5,11,16,2,16,16,16,5,7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "NUM_QUADRO","COD_ITEM","V9002","V8000","V9012",
                           "FATOR_ANUALIZACAO","DEFLATOR","V8000_DEFLA",
                           "VALOR_ANUAL_EXPANDIDO","COD_IMPUT","RENDA_MONETARIA",
                           "RENDA_NAO_MONETARIA","RENDA_TOTAL","V9004","V9001",
                           "SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_VEICULO,paste0(pathdir,"2008-2009/Dados_20231009/","DESPESA_VEICULO.rds"))
rm(DESPESA_VEICULO)

# REGISTROS: RENDIMENTOS E DEDU«’ES - POF5 / QUADRO 53 (TIPO_REG = 14)
RENDIMENTO_TRABALHO <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_RENDIMENTOS_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,1,2,1,5,11,
                        2,2,1,5,11,5,11,5,11,2,5,11,11,11,
                        11,16,16,16,16,2,16,16,16,3,8,8,2,7,
                        3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "NUM_QUADRO","COD_TIPO_OCUP","V5303","V53042","COD_ITEM",
                           "V8500","V9010","V9011","V5305","COD_ITEM_PREV","V53061",
                           "COD_ITEM_IR","V53062","COD_ITEM_OUTRA","V53063",
                           "FATOR_ANUALIZACAO","DEFLATOR","V8500_DEFLA","V53061_DEFLA",
                           "V53062_DEFLA","V53063_DEFLA","VALOR_ANUAL_EXPANDIDO",
                           "VALOR_PREV_ANUAL_EXPANDIDO","VALOR_IR_ANUAL_EXPANDIDO",
                           "VALOR_OUTRAS_ANUAL_EXPANDIDO","COD_IMPUT","RENDA_MONETARIA",
                           "RENDA_NAO_MONETARIA","RENDA_TOTAL","V53041","V53011",
                           "V53021","COD_IMPUT_OCUP_ATIV","V9001","SEQ_LINHA",
                           "COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(RENDIMENTO_TRABALHO,paste0(pathdir,"2008-2009/Dados_20231009/","RENDIMENTO_TRABALHO.rds"))
rm(RENDIMENTO_TRABALHO)

# REGISTROS: OUTROS RENDIMENTOS - POF5 / QUADROS 54 A 57 (TIPO_REG = 15)
OUTROS_RENDIMENTOS <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_OUTROS_RECI_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,5,11,11,
                        5,2,2,2,5,11,11,16,16,2,16,16,16,
                        7,3,6,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","NUM_SEQ","NUM_DV","NUM_DOM","NUM_UC",
                           "COD_INFORMANTE","ESTRATO_POF","PESO","PESO_FINAL",
                           "NUM_QUADRO","COD_ITEM","V8500","V8501","COD_DEDUCAO",
                           "V9010","V9011","FATOR_ANUALIZACAO","DEFLATOR","V8500_DEFLA",
                           "V8501_DEFLA","VALOR_ANUAL_EXPANDIDO","VALOR_DEDUCAO_ANUAL_EXPANDIDO",
                           "COD_IMPUT","RENDA_MONETARIA","RENDA_NAO_MONETARIA",
                           "RENDA_TOTAL","V9001","SEQ_LINHA","COD_UPA","TIPO_SITUACAO_REG")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(OUTROS_RENDIMENTOS,paste0(pathdir,"2008-2009/Dados_20231009/","OUTROS_RENDIMENTOS.rds"))
rm(OUTROS_RENDIMENTOS)

# REGISTRO: CONSUMO ALIMENTAR - POF7 / QUADROS 71 E 72 (TIPO_REG = 16)
CONSUMO_ALIMENTAR <- 
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_CONSUMO_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,15,15,2,1,2,8,5,
                        7,2,1,8,1,16,16,16,5,8,8,1)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG", "COD_UF", "NUM_SEQ", "NUM_DV", "NUM_DOM",
                           "NUM_UC", "COD_INFORMANTE", "ESTRATO_POF", "PESO",
                           "PESO_FINAL", "NUM_QUADRO", "V9018", "V9015",
                           "V9005", "V9007", "V9001", "V9016",
                           "COD_IMPUT_QUANTIDADE", "QTD_IMPUT", "UTILIZA_FREQUENTEMENTE",
                           "RENDA_MONETARIA", "RENDA_NAO_MONETARIA",
                           "RENDA_TOTAL", "COD_UNIDADE_MEDIDA_FINAL", "GRAMATURA1",
                           "QTD_FINAL", "DIA_SEMANA")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CONSUMO_ALIMENTAR,paste0(pathdir,"2008-2009/Dados_20231009/","CONSUMO_ALIMENTAR.rds"))
rm(CONSUMO_ALIMENTAR)

# REGISTRO - MORADOR / QUALIDADE DE VIDA (TIPO_REG = 17)
MORADOR_QUALI_VIDA <-
  read.fwf(paste0(pathdir,"2008-2009/Dados_20231009/","T_MORADOR_QUALI_VIDA_S.txt") 
           , widths = c(2,2,2,1,6,2,1,2,20,20,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,2,20,20,14,14)
           , na.strings=c(" ")
           , col.names = c("TIPO_REG","COD_UF","ESTRATO_POF","TIPO_SITUACAO_REG",
                           "COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE",
                           "CONTAGEM_PONDERADA","FUNCAO_PERDA","V201","V202","V204",
                           "V205","V206","V207","V209","V210","V211","V212","V214",
                           "V215","V216","V218","V219","V220","V301","V302","V303",
                           "V304","V305","V306","V307","V308","V401","V402","V403",
                           "V501","V502","V503","V504","V505","V506","V602","V603",
                           "V604","V605","V606","V607","V608","V609","V610","V705",
                           "V707","V709","V802","V901","V902","GRANDE_REGIAO","C1",
                           "C2","C3","C4","C5","C6","C7","RENDA_DISP_PC",
                           "RENDA_DISP_PC_DEF","PESO","PESO_FINAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR_QUALI_VIDA,paste0(pathdir,"2008-2009/Dados_20231009/","MORADOR_QUALI_VIDA.rds"))
rm(MORADOR_QUALI_VIDA)

