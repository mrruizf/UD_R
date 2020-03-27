library(fastDummies) ##Dummys
library(dplyr)
library(readr)
library(sqldf)
library(maptools)
library(leaflet)
Casos1 <- read_csv("COVID19/DATOS/Casos1.csv")
names(Casos1)<-c("ID de caso","Fecha de diagnóstico","Ciudad de ubicación","Departamento", "Atención","Edad","Sexo","Tipo","País de procedencia")

Casos1$Atención<-gsub("Casa","casa",Casos1$Atención)
Casos1$Atención<-gsub("Hospital","hospital",Casos1$Atención)
Casos1<-dummy_cols(Casos1,select_columns = c("Atención","Edad","Sexo","Tipo"),remove_first_dummy = F)

Casos1 <- mutate_all(Casos1, funs(toupper))
Casos1$`Ciudad de ubicación`<-iconv(Casos1$`Ciudad de ubicación`,from="UTF-8",to="ASCII//TRANSLIT")
Casos1$`Ciudad de ubicación`<-gsub("Ñ","N",Casos1$`Ciudad de ubicación`)
Casos1$`Ciudad de ubicación`<-gsub("TOLIMA","IBAGUE",Casos1$`Ciudad de ubicación`)
Casos1$Departamento<-gsub("CARTAGENA","BOLIVAR",Casos1$Departamento)
Casos1$Departamento<-gsub("BARRANQUILLA","ATLANTICO",Casos1$Departamento)
Casos1$Departamento<-gsub("SANTA MARTA","MAGDALENA",Casos1$Departamento)

Casos1$Departamento<-iconv(Casos1$Departamento,from="UTF-8",to="ASCII//TRANSLIT")
Casos1$`Ciudad de ubicación`<-gsub("Ñ","N",Casos1$`Ciudad de ubicación`)

colombian_municipalities <- read_csv("COVID19/DATOS/colombian_municipalities.csv")
colombian_municipalities$`Municipality Name`<-gsub("SANTAFE DE BOGOTA","BOGOTA",colombian_municipalities$`Municipality Name`)
colombian_municipalities$`Municipality Name`<-gsub("GUADALAJARA DE BUGA","BUGA",colombian_municipalities$`Municipality Name`)
colombian_municipalities$`Municipality Name`<-gsub("CALIMA","DARIEN",colombian_municipalities$`Municipality Name`)
colombian_municipalities$`Department Name`<-gsub("VALLE DEL CAUCA","VALLE",colombian_municipalities$`Department Name`)
colombian_municipalities$`Municipality Name`<-gsub("RETIRO","EL RETIRO",colombian_municipalities$`Municipality Name`)
colombian_municipalities$`Department Name`[colombian_municipalities$`Municipality Name` == "BOGOTA"]<-"BOGOTA"

colombian_municipalities<-sqldf("SELECT COD_DANE AS cod_dane,`Department Name` AS DEPTO,`Municipality Name` AS MPIO
          FROM  colombian_municipalities 
          group by COD_DANE") 

colombian_municipalities <- rbind(colombian_municipalities, c("63001", "QUINDIO", "ARMENIA"))
colombian_municipalities <- rbind(colombian_municipalities, c("5615", "ANTIOQUIA", "RIONEGRO"))
colombian_municipalities <- rbind(colombian_municipalities, c("76400", "VALLE", "LA UNION"))
colombian_municipalities <- rbind(colombian_municipalities, c("88001", "SAN ANDRES ISLAS", "SAN ANDRES ISLAS"))
colombian_municipalities <- rbind(colombian_municipalities, c("76670", "VALLE", "SAN PEDRO"))

COVID19<-sqldf("SELECT cod_dane, Departamento, MPIO,count(`Ciudad de ubicación`) AS CASOS,
          sum(Atención_casa) AS ATN_CASA, sum(Atención_hospital) AS ATN_HOSP,
          sum(Atención_recuperado) AS ATN_RECUPE, sum(Atención_fallecido) AS FALLECIDO,
          sum(`Edad_0 A 9`) AS G0_9, sum(`Edad_10 A 19`) AS G10_19, sum(`Edad_20 A 29`) AS G20_29,
          sum(`Edad_30 A 39`) AS G30_39, sum(`Edad_40 A 49`) AS G40_49, sum(`Edad_50 A 59`) AS G50_59,
          sum(`Edad_60 A 69`) AS G60_69, sum(`Edad_70 A 79`) AS G70_79, sum(`Edad_80 A 89`) AS G80_89,
          sum(Sexo_F) AS GEN_F, sum(Sexo_M) AS GEN_M, sum(Tipo_IMPORTADO) AS VIRUS_IMPORT,
          sum(Tipo_RELACIONADO) AS VIRUS_RELA, sum(`Tipo_EN ESTUDIO`) AS EN_ESTUDIO
          FROM  Casos1,colombian_municipalities
          ON Casos1.`Ciudad de ubicación`=colombian_municipalities.MPIO and Casos1.Departamento=colombian_municipalities.DEPTO
          group by COD_DANE")

COVID19.D<-sqldf("SELECT Departamento, sum(CASOS) AS CASOS,
          sum(ATN_CASA) AS ATN_CASA, sum(ATN_HOSP) AS ATN_HOSP,
          sum(ATN_RECUPE) AS ATN_RECUPE, sum(FALLECIDO) AS FALLECIDO,
          sum(G0_9) AS G0_9, sum(G10_19) AS G10_19, sum(G20_29) AS G20_29,
          sum(G30_39) AS G30_39, sum(G40_49) AS G40_49, sum(G50_59) AS G50_59,
          sum(G60_69) AS G60_69, sum(G70_79) AS G70_79, sum(G80_89) AS G80_89,
          sum(GEN_F) AS GEN_F, sum(GEN_M) AS GEN_M, sum(VIRUS_IMPORT) AS VIRUS_IMPORT,
          sum(VIRUS_RELA) AS VIRUS_RELA,sum(EN_ESTUDIO) AS EN_ESTUDIO
          FROM  COVID19
          group by Departamento")

########################################################################
####################INTEGRANDO DATOS A INFORMACIÓN ESPACIAL#############
########################################################################

#####################################################################
##############################MUNICIPAL##############################
#####################################################################


MPIOS<-readShapePoly("COVID19/SHAPES/INPUT/MPIOS/MPIOS")
MPIOS@data$cod_dane<-as.numeric(as.character(MPIOS@data$cod_dane))
MPIOS <- merge(MPIOS, COVID19, by='cod_dane')
MPIOS@data[is.na(MPIOS@data)] = 0
View(MPIOS@data)

#####################################################################
############################DEPARTAMENTAL############################
#####################################################################

DEPTOS<-readShapePoly("COVID19/SHAPES/INPUT/DEPTOS/DEPTOS")
DEPTOS@data$nom_depart<-iconv(DEPTOS@data$nom_depart,from="UTF-8",to="ASCII//TRANSLIT")
names(DEPTOS@data)<-"Departamento"
DEPTOS@data$Departamento<-gsub("VALLE DEL CAUCA","VALLE",DEPTOS@data$Departamento)
DEPTOS <- merge(DEPTOS, COVID19.D, by='Departamento')
DEPTOS@data[is.na(DEPTOS@data)] = 0
View(DEPTOS@data)
sum(DEPTOS@data$CASOS)
