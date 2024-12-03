#PREPROCESAMIENTO DE DATOS####
getwd()

##1. Abrir base de datos####
#BASE DE DATOS de la muestra ampliada del Censo de Población y Vivienda 2020 de Ciudad de México#
CENSOrespaldo<-read.csv("DATOS/CENSO_CDMX2020/Personas09.CSV")
CENSOrespaldo->CENSO

##2. Procesamiento de variables de interes####

###2.1 Contornos urbanos de residencia actual y anterior####
####Residencia actual####
#crear id del municipio
class(CENSO$ENT)
class(CENSO$MUN)
as.character(CENSO$ENT)->CENSO$ENT
as.character(CENSO$MUN)->CENSO$MUN
library(stringr)
str_pad(CENSO$ENT,width=2,pad='0')->CENSO$ENT
str_pad(CENSO$MUN,width=3,pad='0')->CENSO$MUN
paste0(CENSO$ENT,CENSO$MUN)->CENSO$CVE_MUN
#asignar contornos
CENSO$MUN_CONT[CENSO$CVE_MUN=="09014"|
                 CENSO$CVE_MUN=="09015"|
                 CENSO$CVE_MUN=="09016"|
                 CENSO$CVE_MUN=="09017"]<-1 #ciudad central
CENSO$MUN_CONT[CENSO$CVE_MUN=="09002"|
                 CENSO$CVE_MUN=="09003"|
                 CENSO$CVE_MUN=="09005"|
                 CENSO$CVE_MUN=="09006"|
                 CENSO$CVE_MUN=="09007"|
                 CENSO$CVE_MUN=="09010"]<-2 #2=primer contorno
CENSO$MUN_CONT[CENSO$CVE_MUN=="09008"|
                 CENSO$CVE_MUN=="09012"|
                 CENSO$CVE_MUN=="09013"]<-3 #segundo contorno
CENSO$MUN_CONT[CENSO$CVE_MUN=="09004"|
                 CENSO$CVE_MUN=="09009"|
                 CENSO$CVE_MUN=="09011"|
                 CENSO$CVE_MUN=="09011"]<-4 #tercer contorno
table(CENSO$MUN_CONT,useNA="ifany")
class(CENSO$MUN_CONT)
as.integer(CENSO$MUN_CONT)->CENSO$MUN_CONT
####Residencia anterior####
#crear id del municipio
class(CENSO$ENT_PAIS_RES_5A)
class(CENSO$MUN_RES_5A)
as.character(CENSO$ENT_PAIS_RES_5A)->CENSO$ENT_PAIS_RES_5A
as.character(CENSO$MUN_RES_5A)->CENSO$MUN_RES_5A
str_pad(CENSO$ENT_PAIS_RES_5A,width=2,pad='0')->CENSO$ENT_PAIS_RES_5A
str_pad(CENSO$MUN_RES_5A,width=3,pad='0')->CENSO$MUN_RES_5A
paste0(CENSO$ENT_PAIS_RES_5A,CENSO$MUN_RES_5A)->CENSO$CVE_MUNr5a
#asignar contornos
CENSO$MUN_CONTr5a[CENSO$CVE_MUNr5a=="09014"|
                    CENSO$CVE_MUNr5a=="09015"|
                    CENSO$CVE_MUNr5a=="09016"|
                    CENSO$CVE_MUNr5a=="09017"]<-1 #ciudad central
CENSO$MUN_CONTr5a[CENSO$CVE_MUNr5a=="09002"|
                    CENSO$CVE_MUNr5a=="09003"|
                    CENSO$CVE_MUNr5a=="09005"|
                    CENSO$CVE_MUNr5a=="09006"|
                    CENSO$CVE_MUNr5a=="09007"|
                    CENSO$CVE_MUNr5a=="09010"]<-2 #2=primer contorno
CENSO$MUN_CONTr5a[CENSO$CVE_MUNr5a=="09008"|
                    CENSO$CVE_MUNr5a=="09012"|
                    CENSO$CVE_MUNr5a=="09013"]<-3 #segundo contorno
CENSO$MUN_CONTr5a[CENSO$CVE_MUNr5a=="09004"|
                    CENSO$CVE_MUNr5a=="09009"|
                    CENSO$CVE_MUNr5a=="09011"|
                    CENSO$CVE_MUNr5a=="09011"]<-4 #tercer contorno
table(CENSO$MUN_CONTr5a,useNA="ifany")
class(CENSO$MUN_CONTr5a)
as.integer(CENSO$MUN_CONTr5a)->CENSO$MUN_CONTr5a

###2.2 Cambio de residencia####
CENSO$MOVRr5a<-ifelse(CENSO$CVE_MUN==CENSO$CVE_MUNr5a,0,1) #0 es no hizo un cambio del municipio de residencia intrametropolitano 1 es que si
class(CENSO$MOVRr5a)
as.integer(CENSO$MOVRr5a)->CENSO$MOVRr5a
table(CENSO$MOVRr5a,useNA="ifany")
###2.3 Tiempo al trabajo en horas####
table(CENSO$TIE_TRASLADO_TRAB,useNA="ifany") #143,282 NA
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==1]<-0.25 #15 minutos
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==2]<-0.5 #30 minutos
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==3]<-1 #60 minutos
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==4]<-2 #120 minutos
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==5]<-4 #240 minutos
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==7]<-0.125 #no se traslada
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==6]<-0 #no es posible determinarlo
CENSO$TTTRhr[CENSO$TIE_TRASLADO_TRAB==9]<-0 #no especificado
CENSO$TTTRhr[is.na(CENSO$TTTRhr)]<-0
table(CENSO$TTTRhr,useNA="ifany")


