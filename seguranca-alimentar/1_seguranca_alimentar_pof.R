





# Limpa área de trabalho
rm(list=ls())

# Instala os pacotes necessarios
library(pacman)
p_load(dplyr, data.table, ggplot2, sf,tidyr,RColorBrewer,readxl, car)


# Armazena o caminho da pasta do Projeto
path <- getwd()

#Indica o caminho dos dados
pathdir <- paste(path, "data/", sep = "/")

# Leitura de dados

pos_estrato <-
  readxl::read_excel(
    paste0(pathdir,"2017_2018/Documentacao_20230713/Pos_estratos_totais.xlsx"),skip=5) # [1]


condicoes_vida <- readRDS(paste0(pathdir,"2017_2018/Dados_20230713/CONDICOES_VIDA.rds"))

domicilio <- readRDS(paste0(pathdir,"2017_2018/Dados_20230713/DOMICILIO.rds"))


# Iniciando a base de trabalho
condicoes_vida_novo <- merge(condicoes_vida,
                   pos_estrato,
                   by.x = "COD_UPA" ,
                   by.y = "COD_UPA(UF+SEQ+DV)") 

rm(pos_estrato,condicoes_vida)


# Limpando as vari?veis que duplicaram no merge
condicoes_vida_novo <- transform(condicoes_vida_novo,
                       ESTRATO_POF.y = NULL,
                       ESTRATO_POF = ESTRATO_POF.x,
                       ESTRATO_POF.x = NULL,
                       uf = NULL)

### Base com vari?veis de domic?lios
# selecionando apenas a vari?vel derivada da EBIA
domicilio <- domicilio[,c("COD_UPA",
                          "NUM_DOM",
                          "V6199")] # EBIA

# Atualiza??o da base de trabalho com a base por domic?lio anterior:
condicoes_vida_novo <- merge(condicoes_vida_novo, domicilio)
rm(domicilio)


# Estimando o total de UCs por p?s estrato projetada para a data da POF
tot_uc <- aggregate(PESO_FINAL~pos_estrato,
                    sum,
                    data=condicoes_vida_novo)

colnames(tot_uc) <- c("pos_estrato",
                      "Freq")


# Carregando pacote survey():
library(survey)			# load survey package (analyzes complex design surveys)

# Especifica??o que faz o R produzir erros-padr?es convervadores no lugar de buggar
options( survey.lonely.psu = "adjust" )
# Equivalente ? op??o MISSUNIT no SUDAAN

sample.pof <- svydesign(id = ~COD_UPA ,
                        strata = ~ESTRATO_POF ,
                        data = condicoes_vida_novo ,
                        weights = ~PESO ,
                        nest = TRUE	)


desenho.pof <- postStratify(design = sample.pof ,
                            ~ pos_estrato ,
                            tot_uc)

rm(sample.pof,condicoes_vida_novo)

# Cria??o de vari?vel auxiliar para as estimativas de preval?ncias
desenho.pof <- transform(desenho.pof,
                         one = 1)

# Especifica??o das vari?veis da EBIA
desenho.pof <- update( desenho.pof ,
                       ebia_pub = factor( V6199,
                                          labels = c("Segurança",
                                                     "Insegurança leve",
                                                     "Insegurança moderada",
                                                     "Insegurança grave")),
                       ebia_insan = car::recode( V6199 ,
                                                 " 1 ='Segurança' ;
                                               c(2,3,4)='Insegurança' " ))

## Desenho amostral de domic?lios

POF_dom <- subset( desenho.pof , 
                   NUM_UC == 1) # S? para as UCs=1

# C?lculo das preval?ncias de (in)seguran?a alimentar nos domic?lios:

# C?lculo todas as categorias nos domic?lios:
prev_dom_g01 <- svymean( ~ factor(ebia_pub),
                         POF_dom, 
                         na.rm=T)

# C?lculo com as categorias da EBIA agregadas nos domic?lios:
prev_dom_g02 <- svymean( ~ factor(ebia_insan),
                         POF_dom, 
                         na.rm=T)

Tab3_c1 <- rbind(prev_dom_g01[1],
                 prev_dom_g02[1],
                 prev_dom_g01[2],
                 prev_dom_g01[3],
                 prev_dom_g01[4])

# C?lculo por situa??o do domic?lio com todas as categorias EBIA nos domic?lios:
prev_dom_g03 <- svyby( ~ factor(ebia_pub),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_dom, 
                       svymean,
                       na.rm=T)

# C?lculo por situa??o do domic?lio com as categorias EBIA agregadas nos domic?lios:
prev_dom_g04 <- svyby( ~ factor(ebia_insan),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_dom, 
                       svymean,
                       na.rm=T)

Tab3_c2_c3 <- rbind(prev_dom_g03[,2],
                    prev_dom_g04[,2],
                    prev_dom_g03[,3],
                    prev_dom_g03[,4],
                    prev_dom_g03[,5])

# Parte referente ? POF e domic?lios particulares da tabela 3 da publica??o: Pesquisa de Or?amentos Familiares 2017-2018: An?lise da seguran?a alimentar no Brasil
EBIA_POF_dom <- data.frame(Total=round(Tab3_c1[,1]*100,1),
                           Urbana=round(Tab3_c2_c3[,1]*100,1),
                           Rural=round(Tab3_c2_c3[,2]*100,1),
                           row.names = c(" Com seguran?a alimentar",
                                         " Com inseguran?a alimentar",
                                         "Leve",
                                         "Moderada",
                                         "Grave"))

# Estimativas dos totais de domic?lios com todas as categorias EBIA nos domic?lios:
cont_tot_dom_g01 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de domic?lios com categorias EBIA agregadas nos domic?lios:
cont_tot_dom_g02 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    levels=c("Insegurança",
                                             "Segurança")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

Tab2_c1 <- rbind(cont_tot_dom_g01[1,2],
                 cont_tot_dom_g02[1,2],
                 cont_tot_dom_g01[2,2],
                 cont_tot_dom_g01[3,2],
                 cont_tot_dom_g01[4,2])

# Estimativas dos totais de domic?lios com todas as categorias EBIA nos domic?lios 
# por situa??o do domic?lio:
cont_tot_dom_g03 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de domic?lios com categorias EBIA agregadas nos domic?lios
# por situa??o do domic?lio:
cont_tot_dom_g04 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    c("Insegurança",
                                      "Segurança"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

Tab2_c2_c3 <- rbind(cont_tot_dom_g03[c(1,5),3],
                    cont_tot_dom_g04[c(1,3),3],
                    cont_tot_dom_g03[c(2,6),3],
                    cont_tot_dom_g03[c(3,7),3],
                    cont_tot_dom_g03[c(4,8),3])

# Parte referente ? POF e domic?lios particulares da tabela 2 da publica??o: Pesquisa de Or?amentos Familiares 2017-2018: An?lise da seguran?a alimentar no Brasil
# Observa??o: alguns valores calculados abaixo divergem ligeiramente dos publicados
# decorrente de ajustes feitos na publica??o para consist?ncia de totais de linhas e colunas 
# como consequ?ncia do arredondamento de valores
cont_tot_dom_POF <- data.frame(Total=round(Tab2_c1[,1]/1000,0),
                               Urbana=round(Tab2_c2_c3[,1]/1000,0),
                               Rural=round(Tab2_c2_c3[,2]/1000,0),
                               row.names = c(" Com seguran?a alimentar",
                                             " Com inseguran?a alimentar",
                                             "Leve",
                                             "Moderada",
                                             "Grave"))


############################################################
## Desenho amostral de moradores

morador <- readRDS(paste0(pathdir,"2017_2018/Dados_20230713/MORADOR.rds"))
##########################################################################

# Desenho para a estimativa dos moradores segundo situa??o de 
# seguran?a alimentar

morador_novo <- 
  merge( morador,
         POF_dom$variables[POF_dom$variables$NUM_UC == 1,], # equivalente ? base de domic?lios
         by=c("COD_UPA",
              "NUM_DOM"))

rm(morador)

# Limpando as vari?veis que duplicaram no merge
morador_novo <- transform(morador_novo,
                          NUM_UC.y = NULL,
                          COD_INFORMANTE.y = NULL,
                          TIPO_SITUACAO_REG.y = NULL,
                          ESTRATO_POF.y = NULL,
                          UF.y = NULL,
                          PESO.y = NULL,
                          PESO_FINAL.y = NULL,
                          RENDA_TOTAL.y = NULL,
                          NUM_UC = NUM_UC.x,
                          COD_INFORMANTE = COD_INFORMANTE.x,
                          TIPO_SITUACAO_REG = TIPO_SITUACAO_REG.x,
                          ESTRATO_POF = ESTRATO_POF.x,
                          UF = UF.x,
                          PESO = PESO.x,
                          PESO_FINAL = PESO_FINAL.x,
                          RENDA_TOTAL = RENDA_TOTAL.x,
                          NUM_UC.x = NULL,
                          COD_INFORMANTE.x = NULL,
                          TIPO_SITUACAO_REG.x = NULL,
                          ESTRATO_POF.x = NULL,
                          UF.x = NULL,
                          PESO.x = NULL,
                          PESO_FINAL.x = NULL,
                          RENDA_TOTAL.x = NULL)

# Desenho b?sico por morador
POF.morador.desenho <- svydesign(id = ~COD_UPA ,
                                 strata = ~ESTRATO_POF ,
                                 data = morador_novo ,
                                 weights = ~PESO ,
                                 nest = TRUE	)

# Prepata??o dos totais de p?s-estratifica??o
tot_morador <- aggregate(PESO_FINAL~pos_estrato,
                         sum,
                         data=morador_novo)
colnames(tot_morador) <- c("pos_estrato","Freq")

# Desnho final p?s-estratificado:
POF_morad <- postStratify( design = POF.morador.desenho ,
                           ~ pos_estrato ,
                           tot_morador)

# Confer?ncia de total
POF_morad <- transform(POF_morad,
                       one = 1)

#svytotal(~one,
#         POF_morad)

rm(POF.morador.desenho)


######################################################################

# C?lculo das preval?ncias de (in)seguran?a alimentar dos moradores nos domic?lios:

# C?lculo todas as categorias dos moradores nos domic?lios:
prev_mor_g01 <- svymean( ~ factor(ebia_pub),
                         POF_morad, 
                         na.rm=T)

# C?lculo com as categorias da EBIA agregadas dos moradores nos domic?lios:
prev_mor_g02 <- svymean( ~ factor(ebia_insan),
                         POF_morad, 
                         na.rm=T)

Tab3_c4 <- rbind(prev_mor_g01[1],
                 prev_mor_g02[1],
                 prev_mor_g01[2],
                 prev_mor_g01[3],
                 prev_mor_g01[4])

# C?lculo por situa??o do domic?lio com todas as categorias EBIA dos moradores nos domic?lios:
prev_mor_g03 <- svyby( ~ factor(ebia_pub),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_morad, 
                       svymean,
                       na.rm=T)

# C?lculo por situa??o do domic?lio com as categorias EBIA agregadas dos moradores nos domic?lios:
prev_mor_g04 <- svyby( ~ factor(ebia_insan),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_morad, 
                       svymean,
                       na.rm=T)

Tab3_c5_c6 <- rbind(prev_mor_g03[,2],
                    prev_mor_g04[,2],
                    prev_mor_g03[,3],
                    prev_mor_g03[,4],
                    prev_mor_g03[,5])

# Parte referente ? POF e moradores da tabela 3 da publica??o: Pesquisa de Or?amentos Familiares 2017-2018: An?lise da seguran?a alimentar no Brasil
EBIA_POF_morad <- data.frame(Total=round(Tab3_c4[,1]*100,1),
                             Urbana=round(Tab3_c5_c6[,1]*100,1),
                             Rural=round(Tab3_c5_c6[,2]*100,1),
                             row.names = c(" Com segurança alimentar",
                                           " Com insegurança alimentar",
                                           "Leve",
                                           "Moderada",
                                           "Grave"))

# Estimativas dos totais de moradores com todas as categorias EBIA dos moradores nos domic?lios:
cont_tot_mor_g01 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de moradores com categorias EBIA agregadas dos moradores nos domic?lios:
cont_tot_mor_g02 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    levels=c("Insegurança",
                                             "Segurança")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

Tab2_c4 <- rbind(cont_tot_mor_g01[1,2],
                 cont_tot_mor_g02[1,2],
                 cont_tot_mor_g01[2,2],
                 cont_tot_mor_g01[3,2],
                 cont_tot_mor_g01[4,2])

# Estimativas dos totais de moradores com todas as categorias EBIA dos moradores nos domic?lios 
# por situa??o do domic?lio:
cont_tot_mor_g03 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de moradores com categorias EBIA agregadas dos moradores nos domic?lios
# por situa??o do domic?lio:
cont_tot_mor_g04 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    c("Insegurança",
                                      "Segurança"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

Tab2_c5_c6 <- rbind(cont_tot_mor_g03[c(1,5),3],
                    cont_tot_mor_g04[c(1,3),3],
                    cont_tot_mor_g03[c(2,6),3],
                    cont_tot_mor_g03[c(3,7),3],
                    cont_tot_mor_g03[c(4,8),3])

# Parte referente ? POF e moradores da tabela 2 da publica??o: Pesquisa de Or?amentos Familiares 2017-2018: An?lise da seguran?a alimentar no Brasil
# Observa??o: alguns valores calculados abaixo divergem ligeiramente dos publicados
# decorrente de ajustes feitos na publica??o para consist?ncia de totais de linhas e colunas 
# como consequ?ncia do arredondamento de valores
cont_tot_mor_POF <- data.frame(Total=round(Tab2_c4[,1]/1000,0),
                               Urbana=round(Tab2_c5_c6[,1]/1000,0),
                               Rural=round(Tab2_c5_c6[,2]/1000,0),
                               row.names = c(" Com segurança alimentar",
                                             " Com insegurança alimentar",
                                             "Leve",
                                             "Moderada",
                                             "Grave"))

message("Parte referente ? POF e domic?lios particulares da tabela 2 da publica??o- \nPesquisa de Or?amentos Familiares 2017-2018: \nAn?lise da seguran?a alimentar no Brasil")
cont_tot_dom_POF
message("Parte referente ? POF e moradores da tabela 2 da publica??o- \nPesquisa de Or?amentos Familiares 2017-2018: \nAn?lise da seguran?a alimentar no Brasil")
cont_tot_mor_POF
message("Parte referente ? POF e domic?lios particulares da tabela 3 da publica??o- \nPesquisa de Or?amentos Familiares 2017-2018: \nAn?lise da seguran?a alimentar no Brasil")
EBIA_POF_dom
message("Parte referente ? POF e moradores da tabela 3 da publica??o- \nPesquisa de Or?amentos Familiares 2017-2018: \nAn?lise da seguran?a alimentar no Brasil")
EBIA_POF_morad
