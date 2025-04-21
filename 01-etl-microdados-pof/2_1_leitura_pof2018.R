
## Script original disponibilizado juntamente com os microdados da POF 2017-2018

# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf, googledrive, tidyr,RColorBrewer,readxl)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/raw/", sep = "/")

# Etapa 1: Transformar os dados .txr em rds. Script original dispo --------
# nível na pasta Leitura dos dados da POF
# REGISTRO - DOMICILIO
DOMICILIO <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","DOMICILIO.txt")
           , widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "V0201", "V0202", 
                           "V0203", "V0204", "V0205", "V0206", "V0207",
                           "V0208", "V0209", "V02101", "V02102",
                           "V02103", "V02104", "V02105", "V02111",
                           "V02112", "V02113", "V0212", "V0213",
                           "V02141", "V02142", "V0215", "V02161", 
                           "V02162", "V02163", "V02164", "V0217", 
                           "V0219", "V0220", "V0221", "PESO",
                           "PESO_FINAL", "V6199")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DOMICILIO,paste0(pathdir,"2017-2018/Dados_20230713/","DOMICILIO.rds"))
rm(DOMICILIO)

# REGISTRO - MORADOR
MORADOR <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","MORADOR.txt")
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10,1,20,20,20,20)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", 
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V0306", "V0401", "V04021", "V04022", "V04023",
                           "V0403", "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411", "V0412",
                           "V0413", "V0414", "V0415", "V0416", 
                           "V041711", "V041712", "V041721", "V041722",
                           "V041731", "V041732", "V041741", "V041742",
                           "V0418", "V0419", "V0420", "V0421", "V0422",
                           "V0423", "V0424", "V0425", "V0426", "V0427",
                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
                           "NIVEL_INSTRUCAO", "RENDA_DISP_PC","RENDA_MONET_PC",    
                           "RENDA_NAO_MONET_PC","DEDUCAO_PC"   )
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR,paste0(pathdir,"2017-2018/Dados_20230713/","MORADOR.rds"))
rm(MORADOR)

# REGISTRO - MORADOR / QUALIDADE DE VIDA */
MORADOR_QUALI_VIDA <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","MORADOR_QUALI_VIDA.txt")
           , widths = c(2,4,1,9,2,1,2,20,20,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,2,20,20,14,14)
           , na.strings=c(" ")
           , col.names = c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA",           
                           "NUM_DOM","NUM_UC","COD_INFORMANTE","CONTAGEM_PONDERADA",
                           "FUNCAO_PERDA","V201","V202","V204","V205","V206",              
                           "V207","V208","V209","V210","V211","V212","V214","V215",              
                           "V216","V217","V301","V302","V303","V304","V305","V306",              
                           "V307","V308","V401","V402","V403","V501","V502","V503",              
                           "V504","V505","V506","V601","V602","V603","V604","V605",              
                           "V606","V607","V608","V609","V610","V611","V701","V702",              
                           "V703","V704","V801","V802","V901","V902","GRANDE_REGIAO",     
                           "C1","C2","C3","C4","C5","C6","C7","RENDA_DISP_PC",     
                           "RENDA_DISP_PC_SS","PESO","PESO_FINAL" )
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR_QUALI_VIDA,paste0(pathdir,"2017-2018/Dados_20230713/","MORADOR_QUALI_VIDA.rds"))
rm(MORADOR_QUALI_VIDA)

# REGISTRO - ALUGUEL ESTIMADO
ALUGUEL_ESTIMADO <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","ALUGUEL_ESTIMADO.txt")
           , widths = c(2,4,1,9,2,1,2,7,2,10,2,2,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "V9001", "V9002", "V8000", "V9010", "V9011",
                           "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(ALUGUEL_ESTIMADO,paste0(pathdir,"2017-2018/Dados_20230713/","ALUGUEL_ESTIMADO.rds"))
rm(ALUGUEL_ESTIMADO)

# REGISTRO - DESPESA COLETIVA
DESPESA_COLETIVA <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","DESPESA_COLETIVA.txt")
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_COLETIVA,paste0(pathdir,"2017-2018/Dados_20230713/","DESPESA_COLETIVA.rds"))
rm(DESPESA_COLETIVA)

# REGISTRO - SERVI«OS N√O MONET¡RIOS/POF 2
SERVICO_NAO_MONETARIO_POF2 <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","SERVICO_NAO_MONETARIO_POF2.txt")
           , widths = c(2,4,1,9,2,1,2,2,7,2,10,2,2,10,
                        1,12,10,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V8000", "V9010",
                           "V9011", "V1904", "V1905", "DEFLATOR",
                           "V8000_DEFLA", "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(SERVICO_NAO_MONETARIO_POF2,paste0(pathdir,"2017-2018/Dados_20230713/","SERVICO_NAO_MONETARIO_POF2.rds"))
rm(SERVICO_NAO_MONETARIO_POF2)

# REGISTRO - INVENT¡RIO DE BENS DUR¡VEIS
INVENTARIO <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","INVENTARIO.txt") 
           , widths = c(2,4,1,9,2,1,2,2,7,2,
                        2,4,1,14,14,10
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9005", "V9002", "V1404",
                           "V9012", "PESO", "PESO_FINAL","RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(INVENTARIO,paste0(pathdir,"2017-2018/Dados_20230713/","INVENTARIO.rds"))
rm(INVENTARIO)

# REGISTRO - CADERNETA COLETIVA
CADERNETA_COLETIVA <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","CADERNETA_COLETIVA.txt")
           , widths = c(2,4,1,9,2,1,2,3,7,2,10,12,10,1,2,14,14,10,
                        9,4,5,9,5
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V8000", "DEFLATOR",
                           "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL",
                           "V9005", "V9007", "V9009", "QTD_FINAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CADERNETA_COLETIVA,paste0(pathdir,"2017-2018/Dados_20230713/","CADERNETA_COLETIVA.rds"))
rm(CADERNETA_COLETIVA)

# REGISTRO - DESPESA INDIVIDUAL
DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","DESPESA_INDIVIDUAL.txt") 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_INDIVIDUAL,paste0(pathdir,"2017-2018/Dados_20230713/","DESPESA_INDIVIDUAL.rds"))
rm(DESPESA_INDIVIDUAL)

# REGISTRO - SERVI«OS N√O MONET¡RIOS/POF 4
SERVICO_NAO_MONETARIO_POF4 <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","SERVICO_NAO_MONETARIO_POF4.txt")
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2,2,
                        1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", 
                           "COD_INFORMANTE", "QUADRO", "SEQ",
                           "V9001", "V9002", "V8000", "V9010", "V9011",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(SERVICO_NAO_MONETARIO_POF4,paste0(pathdir,"2017-2018/Dados_20230713/","SERVICO_NAO_MONETARIO_POF4.rds"))
rm(SERVICO_NAO_MONETARIO_POF4)

# REGISTRO - RESTRI«√O DE PRODUTOS OU SERVI«OS DE SA⁄DE
RESTRICAO_PRODUTOS_SERVICOS_SAUDE <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","RESTRICAO_PRODUTOS_SERVICOS_SAUDE.txt")
           , widths = c(2,4,1,9,2,1,2,2,
                        2,7,1,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ","V9001",
                           "V9013", "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(RESTRICAO_PRODUTOS_SERVICOS_SAUDE,paste0(pathdir,"2017-2018/Dados_20230713/","RESTRICAO_PRODUTOS_SERVICOS_SAUDE.rds"))
rm(RESTRICAO_PRODUTOS_SERVICOS_SAUDE)

# REGISTRO - RENDIMENTO DO TRABALHO
RENDIMENTO_TRABALHO <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","RENDIMENTO_TRABALHO.txt") 
           , widths = c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,
                        7,7,2,2,3,1,12,10,10,10,10,1,1,14,14,
                        10,4,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SUB_QUADRO",
                           "SEQ", "V9001", "V5302", "V53021", "V5303",
                           "V5304", "V5305", "V5307", "V8500", "V531112",
                           "V531122", "V531132", "V9010", "V9011",
                           "V5314", "V5315", "DEFLATOR", "V8500_DEFLA",
                           "V531112_DEFLA", "V531122_DEFLA",
                           "V531132_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL","V53011","V53061")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(RENDIMENTO_TRABALHO,paste0(pathdir,"2017-2018/Dados_20230713/","RENDIMENTO_TRABALHO.rds"))
rm(RENDIMENTO_TRABALHO)

# REGISTRO - OUTROS RENDIMENTOS
OUTROS_RENDIMENTOS <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","OUTROS_RENDIMENTOS.txt") 
           , widths = c(2,4,1,9,2,1,2,2,2,7,10,10,2
                        ,2,12,10,10,1,1,14,14,10
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V8500", "V8501", "V9010", "V9011",
                           "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(OUTROS_RENDIMENTOS,paste0(pathdir,"2017-2018/Dados_20230713/","OUTROS_RENDIMENTOS.rds"))
rm(OUTROS_RENDIMENTOS)

# REGISTRO - CONDI«’ES DE VIDA
CONDICOES_VIDA <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","CONDICOES_VIDA.txt") 
           , widths = c(2,4,1,9,2,1,2,1,6,5,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V6101", "V6102", "V6103", "V61041", "V61042",
                           "V61043", "V61044", "V61045", "V61046", 
                           "V61051", "V61052", "V61053", "V61054",
                           "V61055", "V61056", "V61057", "V61058",
                           "V61061", "V61062", "V61063", "V61064",
                           "V61065", "V61066", "V61067", "V61068",
                           "V61069", "V610610", "V610611", "V61071",
                           "V61072", "V61073", "V6108", "V6109",
                           "V6110", "V6111", "V6112", "V6113", "V6114",
                           "V6115", "V6116", "V6117", "V6118", "V6119",
                           "V6120", "V6121", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CONDICOES_VIDA,paste0(pathdir,"2017-2018/Dados_20230713/","CONDICOES_VIDA.rds"))
rm(CONDICOES_VIDA)

# REGISTRO - CARACTERÕSTICAS DA DIETA
CARACTERISTICAS_DIETA <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","CARACTERISTICAS_DIETA.txt") 
           , widths = c(2,4,1,9,2,1,2,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,3,3,14,15,10
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "V7101", "V7102",
                           "V71031", "V71032", "V71033", "V71034",
                           "V71035", "V71036", "V71037", "V71038",
                           "V7104", "V71051", "V71052", "V71053",
                           "V71054", "V71055", "V71056", "V71A01",
                           "V71A02", "V72C01", "V72C02", "PESO",
                           "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CARACTERISTICAS_DIETA,paste0(pathdir,"2017-2018/Dados_20230713/","CARACTERISTICAS_DIETA.rds"))
rm(CARACTERISTICAS_DIETA)

# REGISTRO - CONSUMO ALIMENTAR
CONSUMO_ALIMENTAR <- 
  read.fwf(paste0(pathdir,"2017-2018/Dados_20230713/","CONSUMO_ALIMENTAR.txt") 
           , widths = c(2,4,1,9,2,1,2,2,2,4,2,7,3,
                        2,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,2,2,7,9,6,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,15,10,15,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFOR,MANTE", "QUADRO", "SEQ",
                           "V9005", "V9007", "V9001", "V9015",
                           "V9016", "V9017", "V9018", "V9019",
                           "V9020", "V9021", "V9022", "V9023",
                           "V9024", "V9025", "V9026", "V9027",
                           "V9028", "V9029", "V9030",
                           "COD_UNIDADE_MEDIDA_FINAL",
                           "COD_PREPARACAO_FINAL", "GRAMATURA1",
                           "QTD", "COD_TBCA", "ENERGIA_KCAL",
                           "ENERGIA_KJ", "PTN", "CHOTOT", "FIBRA",
                           "LIP", "COLEST", "AGSAT", "AGMONO",
                           "AGPOLI", "AGTRANS", "CALCIO", "FERRO",
                           "SODIO", "MAGNESIO", "FOSFORO", "POTASSIO",
                           "COBRE", "ZINCO", "VITA_RAE", "TIAMINA",
                           "RIBOFLAVINA", "NIACINA", "PIRIDOXAMINA",
                           "COBALAMINA", "VITD", "VITE", "VITC",
                           "FOLATO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL", "DIA_SEMANA", "DIA_ATIPICO")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(CONSUMO_ALIMENTAR,paste0(pathdir,"2017-2018/Dados_20230713/","CONSUMO_ALIMENTAR.rds"))
rm(CONSUMO_ALIMENTAR)
