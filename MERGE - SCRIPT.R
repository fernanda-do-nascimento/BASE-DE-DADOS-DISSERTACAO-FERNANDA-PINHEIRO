table(BASE2020$DS_SIT_TOT_TURNO)


#Precisaremos do pacote 
library(writexl)

#salvando
writexl::write_xlsx(BASE2020, path = "BASE2020.xlsx")
writexl::write_xlsx(BASE2016, path = "BASE2016.xlsx")

save(BASE2016, file = "BASE2016.RData")
save(BASE2020, file = "BASE2020.RData")


#remover os acentos...
receita_sen$NOME <- gsub("[^a-zA-Z0-9 ]", "", 
                         iconv(receita_sen$NOME, 
                               to = "ASCII//TRANSLIT"))

##Recodificações####
#2020####
LISTA_CANDIDATOS_2020$NM_URNA_CANDIDATO <- 
  LISTA_CANDIDATOS_2020$`Nome Candidata(o)`

LISTA_CANDIDATOS_2020$NOME_URNA <- gsub("[^a-zA-Z0-9 ]", "", 
                         iconv(LISTA_CANDIDATOS_2020$NM_URNA_CANDIDATO, 
                               to = "ASCII//TRANSLIT"))

BASE2020$NOME_URNA <- gsub("[^a-zA-Z0-9 ]", "", 
    iconv(BASE2020$NM_URNA_CANDIDATO, 
               to = "ASCII//TRANSLIT"))

BASE2020$NOME_CANDIDATO <- gsub("[^a-zA-Z0-9 ]", "", 
                           iconv(BASE2020$NM_CANDIDATO, 
                                 to = "ASCII//TRANSLIT"))

#2016####


LISTA_DE_CANDIDATOS_2016$NOME_URNA <- gsub("[^a-zA-Z0-9 ]", "", 
             iconv(LISTA_DE_CANDIDATOS_2016$NM_URNA, 
      to = "ASCII//TRANSLIT"))


BASE2016$NOME_URNA <- gsub("[^a-zA-Z0-9 ]", "", 
        iconv(BASE2016$NM_URNA_CANDIDATO, 
     to = "ASCII//TRANSLIT"))

BASE2016$NOME_CANDIDATO <- gsub("[^a-zA-Z0-9 ]", "", 
                           iconv(BASE2016$NM_CANDIDATO, 
                                 to = "ASCII//TRANSLIT"))


#################MERGE###################
#2020####
colnames(BASE2020)
BASE2020$SG_UF <- BASE2020$SG_UF.x.x

table(BASE2020$SG_UF)
table(LISTA_CANDIDATOS_2020$Estado)

LISTA_CANDIDATOS_2020$SG_UF <- LISTA_CANDIDATOS_2020$Estado

LISTA_CANDIDATOS_2020$NR_CANDIDATO <- LISTA_CANDIDATOS_2020$Número

BASE2020$SG_UF <- BASE2020$SG_UF.x.x



BASELGBT2020 <- merge(LISTA_CANDIDATOS_2020, BASE2020, 
                      by = c("NOME_URNA", "NR_CANDIDATO"), all.x = F)

save(BASELGBT2020, file="BASELGBT2020.RData")

BASELGBT2020.1<- merge(LISTA_CANDIDATOS_2020, BASE2020, 
                        by = c("NOME_URNA", "NR_CANDIDATO"), all.x = T)

save(BASELGBT2020.1, file="BASELGBT2020.1.RData")

writexl::write_xlsx(BASELGBT2020.1, 
                    path = "BASELGBT2020.xlsx")

#CONFERÊNCIA 2020####
#  anti_join para encontrar as observações sem correspondência
LISTA_CANDIDATOS_2020$NOME_URNA <- LISTA_CANDIDATOS_2020$`Nome Candidata(o)`

sem_correspondencia2020<- 
  anti_join(LISTA_CANDIDATOS_2020, 
            BASELGBT2020, by = c("NOME_URNA", "NR_CANDIDATO"))

save(sem_correspondencia2020, file = "sem_correspondencia2020.RData")

#Vou reduzir a base2020 para salvar em excel

Estado <- sem_correspondencia2020$Estado

BASE2020CONFERENCIA <- subset(BASE2020, SG_UF %in% Estado)

Partido <- sem_correspondencia2020$`Nome partido`

BASE2020CONFERENCIA <- subset(BASE2020, NM_PARTIDO %in% Partido)


writexl::write_xlsx(BASE2020CONFERENCIA, 
                    path = "BASE2020CONFERENCIA.xlsx")

#
library(dplyr)

# 
BASELGBT2020.3 <- LISTA_CANDIDATOS_2020 %>% 
  left_join(BASE2020, by= "NOME_URNA")

#2020 vamos ter que recuperar o número do candidato
#Depois juntar as bases considerando a coluna LGBT no total para saber do total
#quantos são gays
#Depois organizar os ajustes que devem ser feitos para a Fernanda


#2016####
BASE2016$NM_CANDIDATO.x 

LISTA_DE_CANDIDATOS_2016$NM_CANDIDATO.x  <- 
  LISTA_DE_CANDIDATOS_2016$NM_CANDIDATO



BASE2016$NM_VOTAVEL <- BASE2016$NR_CANDIDATO.x
BASE2016$NM_VOTAVEL <- as.factor(BASE2016$NM_VOTAVEL)

BASELGBT2016 <- merge(LISTA_DE_CANDIDATOS_2016, BASE2016, 
                        by = c("NM_CANDIDATO.x", "NM_VOTAVEL"),  all.x = T)

save(BASELGBT2016, file = "BASELGBT2016.RData")

BASELGBT2016.1 <- merge(LISTA_DE_CANDIDATOS_2016, BASE2016, 
                      by = c("NM_CANDIDATO.x", "NM_VOTAVEL"),  all.x = F)

save(BASELGBT2016.1, file = "BASELGBT2016.1.RData") 


writexl::write_xlsx(BASELGBT2016, path = "BASELGBT2016.xlsx")



#CONFERÊNCIA 2016####
#  anti_join para encontrar as observações sem correspondência
386-372

sem_correspondencia2016 <- 
  anti_join(LISTA_DE_CANDIDATOS_2016, 
            BASE2016, by = c("NM_CANDIDATO.x", "NM_VOTAVEL"))

save(sem_correspondencia2016, file = "sem_correspondencia2016.RData")

table(sem_correspondencia2016$SG_UF)

#reduzir a base 

Partido <- sem_correspondencia2016$SIGLA_PARTIDO
BASE2016$SIGLA_PARTIDO <- BASE2016$SG_PARTIDO

BASE2016CONFERENCIA <- subset(BASE2016, SIGLA_PARTIDO %in% Partido)

writexl::write_xlsx(BASE2016CONFERENCIA, 
                    path = "BASE2016CONFERENCIA.xlsx")



#########################
#Antra#####
#FALTANTES112####
ANTRA_FALTANTES$NM_URNA_CANDIDATO <- ANTRA_FALTANTES$NOME

ANTRA_FALTANTES$NM_URNA_CANDIDATO <- gsub("[^a-zA-Z0-9 ]", "", 
      iconv(ANTRA_FALTANTES$NM_URNA_CANDIDATO, 
          to = "ASCII//TRANSLIT"))

########
#Merge ANTRA####
#2020####
colnames(BASE2020)
BASE2020$UF <- BASE2020$SG_UF.x.x

table(BASE2020$UF)

table(ANTRA_FALTANTES$UF)
# AL AM AP BA CE ES GO MA MG MS PE PI PR RJ RN RO RR SP TO 
# 4  3  1 24 11  6 10  3 33  1  1  3  5 15 12  5  2 49  1 

# Excluir ESTADOS QUE NÃO TEM 
# Defina as condições
excluir <- BASE2020$UF %in% 
  c("AC", "MT", "PA", "PB",
    "RS", "SC", "SE")

BASE2020 <- BASE2020[!excluir, ]
table(BASE2020$UF)

# AL    AM    AP    BA    CE    ES    GO    MA    MG    MS    PE    PI    PR    RJ    RN 
# 6991  9873  2292 38987 15021 11745 23381 19543 78678  8220 19786 10219 35276 24391  9943 
# RO    RR    SP    TO 
# 5621  1792 88166  8294

BASE2020$CIDADE <- BASE2020$NM_UE

table(BASE2020$CIDADE)


BASEcomplementar2020 <- merge(ANTRA_FALTANTES, BASE2020, 
                      by = c("NM_URNA_CANDIDATO", "UF", "CIDADE"), all.x = F)

BASEcomplementar2020.1 <- merge(ANTRA_FALTANTES, BASE2020, 
                              by = c("NM_URNA_CANDIDATO", "UF", "CIDADE"), all.x = T)

save(BASEcomplementar2020, file = "BASEcomplementar2020.RData")

save(BASEcomplementar2020.1, file = "BASEcomplementar2020.1.RData")

writexl::write_xlsx(BASEcomplementar2020, 
                    path = "BASEcomplementar2020.xlsx")

writexl::write_xlsx(BASEcomplementar2020.1, 
                    path = "BASEcomplementar2020.1.xlsx")




table(BASE2020$SG_UF)
table(LISTA_CANDIDATOS_2020$Estado)

LISTA_CANDIDATOS_2020$SG_UF <- LISTA_CANDIDATOS_2020$Estado

LISTA_CANDIDATOS_2020$NR_CANDIDATO <- LISTA_CANDIDATOS_2020$Número

BASE2020$SG_UF <- BASE2020$SG_UF.x.x



BASELGBT2020 <- merge(LISTA_CANDIDATOS_2020, BASE2020, 
                      by = c("NOME_URNA", "NR_CANDIDATO"), all.x = F)

save(BASELGBT2020, file="BASELGBT2020.RData")

BASELGBT2020.1<- merge(LISTA_CANDIDATOS_2020, BASE2020, 
                       by = c("NOME_URNA", "NR_CANDIDATO"), all.x = T)

save(BASELGBT2020.1, file="BASELGBT2020.1.RData")

writexl::write_xlsx(BASELGBT2020.1, 
                    path = "BASELGBT2020.xlsx")


#FALTANTES77#### 
#V - ACHOU 33 PORÉM 2 ERA A
ANTRA_FALTANTES <- BASEcomplementar2020_1


ANTRA_FALTANTES$NM_URNA_CANDIDATO <- gsub("[^a-zA-Z0-9 ]", "", 
                                          iconv(ANTRA_FALTANTES$NM_URNA_CANDIDATO, 
                                                to = "ASCII//TRANSLIT"))

ANTRA_FALTANTES$CIDADE <- gsub("[^a-zA-Z0-9 ]", "", 
                                          iconv(ANTRA_FALTANTES$CIDADE, 
                                                to = "ASCII//TRANSLIT"))



########
#Merge ANTRA####
#2020####

BASE2020$CIDADE <- gsub("[^a-zA-Z0-9 ]", "", 
                               iconv(BASE2020$CIDADE, 
                                to = "ASCII//TRANSLIT"))


colnames(BASE2020)
BASE2020$UF <- BASE2020$SG_UF.x.x

table(BASE2020$UF)

table(ANTRA_FALTANTES$UF)
# AL AP BA CE ES GO MA MG MS PI PR RJ RN RO SP 
# 3  1  7  7  1  5  1 13  1  2  3  8  6  1 18 

# Excluir ESTADOS QUE NÃO TEM 
# Defina as condições
excluir <- BASE2020$UF %in% 
  c("AC", "AM", "MT", "PA",
    "PB", "PE", "RR", "RS", "SC", "SE", "TO")

BASE2020 <- BASE2020[!excluir, ]
table(BASE2020$UF)

# AL    AP    BA    CE    ES    GO    MA    MG    MS    PI    PR    RJ    RN    RO    SP 
# 6991  2292 38987 15021 11745 23381 19543 78678  8220 10219 35276 24391  9943  5621 88166 

BASE2020$CIDADE <- BASE2020$NM_UE

#Filtrar a base apenas pelas cidades dos 77 casos
table(BASE2020$CIDADE)

table(ANTRA_FALTANTES$CIDADE)

#AMARGOSA,ARARAQUARA,ASSU,BATATAIS,BELO HORIZONTE,CAICO,CAMPO GRANDE,CAMPO LIMPO PAULISTA,CARNAUBA DOS DANTAS,COLONIA,DIONISIO,DUQUE DE CAXIAS,EUNAPOLIS,FEIRA DE SANTANA,GOIANIA,GUIUBA,IGUATU,IPAMERI,ITABIRITO,JUAZEIRO DO NORTE,JUIZ DE FORA,MACAPA,MACEIO,MARIANA,MARILIA,MONTE BELO,MOSSORO,NITEROI,NOVA FRIBURGO,NOVA RUSSAS,OCAUCU,OURO PRETO,PAPAGAIOS,PONTAL DO PARANA,PORTO SEGURO,PORTO VELHO,QUIRINOPOLIS,RIBEIRAO CLARO,RIO CLARO,RIO DE JANEIRO,SALVADOR,SANTA CRUZ,SANTA LUZIA DO NORTE,SANTANA DO MARANHAO,SANTO ESTAVAO,SAO CAETANO DO SUL,SAO CARLOS,SAO GONCALO,SAO JOAO DEL REY,SAO JOSE DO DIVINO,SAO JOSE DO RIO PRETO,SAO JOSE DOS CAMPOS,SAO LUIS DO CURU,SAO MIGUEL DO ARAGUAIA,SAO PAULO,SAO PEDRO DALDEIA,TAQUARIVAI,TATUI,UBERLANDIA,UBIRATA,VESPASIANO,VILA VELHA


library(dplyr)
BASE2020$CIDADE

BASE2020.77 <- filter(BASE2020, CIDADE %in% c("AMARGOSA",
                      "ARARAQUARA","ASSU","BATATAIS","BELO HORIZONTE",
                      "CAICO","CAMPO GRANDE","CAMPO LIMPO PAULISTA",
                      "CARNAUBA DOS DANTAS","COLONIA","DIONISIO",
                      "DUQUE DE CAXIAS","EUNAPOLIS","FEIRA DE SANTANA",
                      "GOIANIA","GUIUBA","IGUATU","IPAMERI",
                      "ITABIRITO","JUAZEIRO DO NORTE",
                      "JUIZ DE FORA","MACAPA","MACEIO","MARIANA",
                      "MARILIA","MONTE BELO",'MOSSORO','NITEROI',
                      "NOVA FRIBURGO","NOVA RUSSAS","OCAUCU",
                      "OURO PRETO","PAPAGAIOS","PONTAL DO PARANA",
                      "PORTO SEGURO","PORTO VELHO","QUIRINOPOLIS",
                      "RIBEIRAO CLARO","RIO CLARO","RIO DE JANEIRO",
                      "SALVADOR","SANTA CRUZ","SANTA LUZIA DO NORTE",
                      "SANTANA DO MARANHAO","SANTO ESTAVAO","SAO CAETANO DO SUL",
                      "SAO CARLOS","SAO GONCALO","SAO JOAO DEL REY",
                      "SAO JOSE DO DIVINO","SAO JOSE DO RIO PRETO",
                      "SAO JOSE DOS CAMPOS","SAO LUIS DO CURU",
                      "SAO MIGUEL DO ARAGUAIA","SAO PAULO",
                      "SAO PEDRO DALDEIA","TAQUARIVAI","TATUI",
                      "UBERLANDIA","UBIRATA","VESPASIANO","VILA VELHA"))


#Filtrar a base apenas pelos partidos
table(ANTRA_FALTANTES$PARTIDO)

#AVANTE,DEM,MDB,PATRIOTA,PCDOB,PDT,PL,PMB,PMN,PODEMOS,PP,PROGRESSISTAS,PROS,PRTB,PSB,PSC,PSD,PSDB,PSOL,PT,PTB,PTC,PV,REDE,REPUBLICANOS,SOLIDARIEDADE,UNIDADE POPULAR


library(dplyr)
table(BASE2020.77$SG_PARTIDO)

BASE2020.77 <- filter(BASE2020.77, SG_PARTIDO %in% c("AVANTE","DEM","MDB",
                                              "PATRIOTA","PC do B","PDT","PL",
                                              "PMB","PMN","PODE","PP","PROS",
                                              "PRTB","PSB","PSC","PSD","PSDB","PSOL",
                                              "PT","PTB","PTC","PV","REDE","REPUBLICANOS",
                                              "SOLIDARIEDADE","UP"))
table(BASE2020.77$SG_PARTIDO)

#VOU TRANSFORMAR O NOME DE URNA DA ANTRA EM NOME 2, VOU TIRAR OS ACENTOS 
#VOU TENTAR JUNTAR NOVAMENTE PELO NOME DE URNA


ANTRA_FALTANTES$NOME2 <- gsub("[^a-zA-Z0-9 ]", "", 
                            iconv(ANTRA_FALTANTES$NOME, 
                                     to = "ASCII//TRANSLIT"))

BASE2020.77$NOME2 <-  BASE2020.77$NOME_URNA

BASEcomplementar2020.2 <- merge(ANTRA_FALTANTES, BASE2020, 
                              by = c("NOME2", 
                                     "UF"), all.x = F)

BASEcomplementar2020.3 <- merge(ANTRA_FALTANTES, BASE2020, 
                                by = c("NOME2", 
                                       "UF"), all.x = T)

writexl::write_xlsx(BASEcomplementar2020.3 , 
                    path = "BASEcomplementar2020.3.xlsx")


#FALTANTES44#### 

ANTRA_FALTANTES <- BASEcomplementar2020_3


#Filtrar pela uf de novo
table(ANTRA_FALTANTES$UF)

# AL BA CE ES GO MA MG MS PI PR RJ RN RO SP 
# 1  3  2  1  4  1  7  1  2  1  7  3  1 10


table(BASE2020.77$UF)

BASE2020.77 <- filter(BASE2020.77, UF %in% c("AL", 
"BA", "CE", "ES", "GO", "MA", "MG", "MS", "PI", "PR", "RJ", "RN", "RO", "SP"))


#Filtrar a base apenas pelos partidos
table(ANTRA_FALTANTES$PARTIDO)

library(dplyr)
table(BASE2020.77$SG_PARTIDO)

BASE2020.77 <- filter(BASE2020.77, SG_PARTIDO %in% c("AVANTE","DEM","MDB",
                                                     "PDT", "PMB","PMN","PODE","PP","PROS",
                                                     "PSB","PSD","PSDB","PSOL",
                                                     "PT","PTC","PV","REDE",
                                                     "SOLIDARIEDADE"))
table(BASE2020.77$SG_PARTIDO)

#salvar em xlsx


writexl::write_xlsx(BASE2020.77, 
                    path = "BASEfalta44.xlsx")
#SEPAREI OS NOMES PARA FICAR IGUAL A BASE DA ANTRA COM O 1º NOME EM UMA COLUNA SEPARADA


#JUNTAR
BASECOMPLEMENTAR <- BASEcomplementar2020_3


BASECOMPLEMENTAR$CIDADE
BASECOMPLEMENTAR$UF
BASECOMPLEMENTAR$NOME2
BASECOMPLEMENTAR$PARTIDO

BASEfalta44$UF
BASEfalta44$CIDADE
BASEfalta44$NOME2...8

BASEfalta44$PARTIDO <- BASEfalta44$NM_PARTIDO

BASEfalta44$NOME2 <- BASEfalta44$NOME2...8

BASEfalta44$NR_CANDIDATO
BASECOMPLEMENTAR$NR

BASEfalta44$NR <- BASEfalta44$NR_CANDIDATO

BASEC.4 <- merge(BASECOMPLEMENTAR, BASEfalta44, 
                                by = c("NR", "UF", "CIDADE",
                                       "PARTIDO"), all.x = F)

BASEC.5 <- merge(BASECOMPLEMENTAR, BASEfalta44,
                                by = c("NOME2", 
                                       "UF", "CIDADE",
                                       "PARTIDO"), all.x = T)

BASEC.4 <- merge(BASECOMPLEMENTAR, BASEfalta44, 
                 by = c("NOME2", 
                        "UF", "CIDADE"), all.x = F)

writexl::write_xlsx(BASEcomplementar2020.3 , 
                    path = "BASEcomplementar2020.3.xlsx")



#OS DEMAIS NAO LOCALIZADOS FORAM BUSCADOS MANUALMENTE NA BASE GERAL DE CANDIDATOS E INCORPORADOS NA BASE FINAL.

#AO LONGO DO TEMPO, FOMOS EXECUTANDO LIMPEZAS E CONFERÊNCIAS PARA CERTIFICAR A LEGITIMIDADE DOS DADOS.