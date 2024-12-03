#ANÁLISIS####

##1. Seleccionar poblacion de interés####
CENSO[CENSO$EDAD>=25&CENSO$EDAD<=39,]->CENSOaj
CENSOaj[CENSOaj$MOVRr5a==1,]->CENSOajmov


##2. Estadisticas descriptivas####
#para explorar las variables de interés

####2.1 tablas de frecuencias####
table(CENSOaj$MUN_CONT,useNA="ifany") #contorno de residencia actual
table(CENSOaj$MUN_CONTr5a,useNA="ifany") #contorno de residencia anterior
table(CENSOaj$MOVRr5a,useNA="ifany") #cambio de residencia
table(CENSOaj$TTTRhr,useNA="ifany") #tiempo al trabajo en horas

####2.2 medias y medianas####
median(CENSOaj$MUN_CONT) #contorno de residencia actual
median(CENSOaj$MUN_CONTr5a,na.rm=TRUE) #contorno de residencia anterior
median(CENSOaj$MOVRr5a) #cambio de residencia

mean(CENSOaj$TTTRhr[is.na(CENSOaj$MUN_CONTr5a)==FALSE]) #tiempo al trabajo en horas de los adultos jovenes
mean(CENSOajmov$TTTRhr[is.na(CENSOajmov$MUN_CONTr5a)==FALSE]) #tiempo al trabajo en horas de los adultos jovenes que se mueven


##3. Analisis comparativo####
#de los tiempos de traslado al trabajo entre población adulta joven de referencia y los que se movieron

###3.1 Matriz de los adultos jovenes#### 
#primero se calcula la matriz de la suma de los tiempos agregando los margenes de las columnas y las filas
tapply(CENSOaj$TTTRhr
       , list(CENSOaj$MUN_CONT,CENSOaj$MUN_CONTr5a)
       , sum)->cuadro_suma
cuadro_suma
addmargins(cuadro_suma)->cuadro_suma
cuadro_suma
#segundo se calcula el conteo de las observaciones agregando los margenes de las columnas y las filas
addmargins(table(CENSOaj$MUN_CONT,CENSOaj$MUN_CONTr5a,CENSOaj$TTTRhr))[,,"Sum"]->cuadro_nobservaciones
cuadro_nobservaciones
#tercero se calcula las medias 
cuadro_suma/cuadro_nobservaciones->resultado1
resultado1

###3.2 Matriz de los adultos jovenes que se movieron#### 
#primero se calcula la matriz de la suma de los tiempos agregando los margenes de las columnas y las filas
tapply(CENSOajmov$TTTRhr
       , list(CENSOajmov$MUN_CONT,CENSOajmov$MUN_CONTr5a)
       , sum)->cuadro_suma
cuadro_suma
addmargins(cuadro_suma)->cuadro_suma
cuadro_suma
#segundo se calcula el conteo de las observaciones agregando los margenes de las columnas y las filas
addmargins(table(CENSOajmov$MUN_CONT,CENSOajmov$MUN_CONTr5a,CENSOajmov$TTTRhr))[,,"Sum"]->cuadro_nobservaciones
cuadro_nobservaciones
#tercero se calcula las medias 
cuadro_suma/cuadro_nobservaciones->resultado2
resultado2


##4. Comparacion####
resultado1
resultado2


##5. Presentacion####
colnames(resultado1)<-c("Cd. central","Contorno 1","Contorno 2","Contorno 3","Salen")
rownames(resultado1)<-c("Cd. central","Contorno 1","Contorno 2","Contorno 3","Llegan")
round(resultado1,3)->resultado1
resultado1

colnames(resultado2)<-c("Cd. central","Contorno 1","Contorno 2","Contorno 3","Salen")
rownames(resultado2)<-c("Cd. central","Contorno 1","Contorno 2","Contorno 3","Llegan")
round(resultado2,3)->resultado2
resultado2


##6. Guardar####
setwd("RESULTADOS/")
write.csv(resultado1, "resultado1.csv")
write.csv(resultado2, "resultado2.csv")


