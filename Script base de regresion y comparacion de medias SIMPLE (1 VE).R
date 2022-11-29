# 1) Estudio experimental 
# Cantidad de replicas: 3
# Vr: ID del adn (cuantitativa continua, con potencial distribucion normal)
# Ve: Dosis de glifosato (cuantitativa continua, efectos fijos) con 6 niveles (0, 500, 750, 1000, 1250, 1750 ug/huevo)

ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecute 
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
#seteo el working directory
setwd("")
#levanto la base de datos y creo el objeto datos

datos <- read.delim("Ru.txt")

#primeros analisis exploratorios de la base de datos
names(datos) # Nombre de las variables
dim(datos) #Dimensiones del dataframe
table(datos$RU) #R?plicas

##Tipo de variable
class(datos$RU) 
class(datos$ID)
str(datos) # resume lo anteriortable

#2) describir grafica y descriptivamente los datos

#descriptiva
library(doBy) #descripciones por grupo
summaryBy(ID~RU, data=datos, FUN=c(mean, sd, length))

#obtengo numero de replicas (lenght)
library(dplyr)
datos %>% 
  group_by(RU) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

library(plyr)  #Para ddply
cdata <- ddply(datos, c("RU"), summarise,
               N    = length(ID),
               mean = mean(ID),
               sd   = sd(ID),
               se   = sd / sqrt(N))
cdata #ddply arma dataframe con estad?sticos que le pidas

#grafico 1#

library(ggplot2)
ggplot(datos, aes(x=RU, y=ID))+
  geom_boxplot()+ geom_jitter(width = 0.2)

ggplot(datos, aes(x=RU, y=ID)) + ylab("ID danio al ADN")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), size=1, 
               geom="pointrange", color="darkblue") +
  geom_jitter(alpha=0.3, size=2, position = position_jitter(width = .2))

#GRAFICO DE DISPERSION#

disp <- ggplot(datos, aes(x=RU, y=ID)) +    #Grafico de dispersion para ver la relaci?n entre la variable explicatoria y la variable respuesta
  geom_point(size=3, color="red", shape=19) +  
  geom_smooth(method=lm, se=F, fullrange=F, size=0.5)+ 
  xlab("Conc. Herbicida suministrada(mg/huevo)") +  ylab("ID ") 
disp

#Se observa que a medida que aumenta el RU, aumentan tambien los danios en el DNA.
#Pareceria existir una relacion lineal y positiva.

#3) Analice el da?o sobre el ADN en funci?n de la concentracion del herbicida. Plantee el/los modelo/s, compruebe los
#supuestos. Realice este procedimiento con la funcion lm(). Primero, considerando a la variable RU[/mug/huevo]
#como factor. Luego, considerando a la variable RU[?g/huevo] como numerica.

##########################################################################
#RU COMO FACTOR    ID_ij= mu + alfa_i + E_ij (Eij~NID(0,Var)), i=1-6 j=1-3
##########################################################################
class(datos$RU)
datos$RU <- as.factor (datos$RU)
m1 <- lm(ID~RU, datos)
#Verifico los supuestos de normalidad y homogeneidad de varianzas.
#Calculamos los residuos y los predichos

e<-residuals(m1) # residuos
re<-rstandard(m1) #residuos estandarizados
pre<-predict(m1) #predichos

install.packages("carData") #para Levene y QQ-plot
library(car)
par(mfrow = c(1, 2))

# Grafico de residuos estandarizados vs valores predichos por el modelo
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersion de RE vs PRED" )
abline(0,0) 

#No se ven patrones en los residuos vs predichos, se ve un "cielo estrellado" por lo que no rechazamos homocedasticidad.

# QQ plot 
qqPlot(e, main = "QQ Plot residuos") #No parece apartarse de la normalidad.

par(mfrow = c(1, 1))

# Prueba de normalidad (Shapiro-Wilk test of normality)
shapiro.test(e) #H0: Normalidad de los residuos. p-value = 0.072 No hay evidencias para rechazar normalidad.


# Se puede testear la homogeneidad de varianzas (comparacion de medias con replicas)

leveneTest(m1) #H0: Homocedasticidad de los residuos. p-valor:0.9856 No hay evidencias para rechazar homocedasticidad.

#Se cumplen los supuestos del modelo (ademas deben cumplirse los supuestos de disenio, independencia en las obs)
#Ahora puedo hacer prueba estadictica para el modelo. 
anova(m1) 
#H0: No difieren las medias entre niveles del factor.
#Ha: Al menos una de las medias difiere del resto. 
# p-valor:2.217e-06 *** Rechazo H0

#Hago contrastes a posteriori con Tukey para saber que media difiere de cual.
# Analizar la magnitud del efecto
library(emmeans) #necesaria para hacer los contrastes.

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),contrast = list(infer = c(TRUE, TRUE))))

# comparaciones
comp <- emmeans(m1, pairwise ~ RU)
comp


# comparaciones y plot
comp$contrasts
plot(comp$emmeans, comparisons = TRUE)
#COMENTARIO.

# medias 
confint(comp$emmeans) #pedimos los intervalos de confianza para las medias

#Por un lado tengo el IC para la media de cada tratamiento y a partir de los contrastes tengo el IC para la diferencia entre las medias de pares de tratamientos. 
# Eso en el plot se ve como las bandas azules en el IC de las medias y las flechitas rojas para la diferencia de medias


########################################################################
# RU COMO NUMERICA IDij= B0+ B1*RU_i + Eij (Eij~NID(0,Var)) i=1-6 j=1-3
########################################################################

class(datos$RU)
datos$RU <- as.numeric (datos$RU)
m2 <- lm(ID~RU, datos) #Modelo de Regresion Lineal

#HIP?TESIS DEL MODELO:
#Ho: B1=0. La variaci?n de Y no se explica linealmente por la variaci?n de X
#Ha: B1 diferente de 0. La variacion de Y si se explica linealmente por la variacion de X

#Verifico los supuestos de normalidad, linealidad y homogeneidad de varianzas.
#Calculamos los residuos y los predichos

e<-residuals(m2) # residuos
re<-rstandard(m2) #residuos estandarizados
pre<-predict(m2) #predichos

install.packages("carData") #para Levene y QQ-plot
library(car)
par(mfrow = c(1, 2))

# Grafico de residuos estandarizados vs valores predichos por el modelo
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersion de RE vs PRED" )
abline(0,0) 

#No se ven patrones en los residuos vs predichos, se ve un "cielo estrellado" por lo que no rechazamos homocedasticidad.
#Ademas en este grafico se puede observar que se cumple el supuesto de linealidad. No se ven patrones tipo "sonrisa"

# QQ plot 
qqPlot(e, main = "QQ Plot residuos") #No parece apartarse de la normalidad.

par(mfrow = c(1, 1))

#Evaluacion analitica de normalidad
# Prueba de normalidad (Shapiro-Wilk test of normality)
shapiro.test(e) #H0: Normalidad de los residuos.  p-value = 0.5369 No rechazo normalidad.No hay evidencia suficiente para concluir que la distribuci?n no es normal


# No se puede testear la homogeneidad de varianzas utilizando Levene Test (regresion con VE cuantitativa)


#Se cumplen los supuestos del modelo (ademas deben cumplirse los supuestos de disenio, independencia en las obs)
#Ahora puedo hacer prueba estadictica para el modelo. 
summary(m2) #H0: B1=0 (No hay magnitud de efecto de la VE sobre la VR) Rechazo H0. p-valor:9.438 6.11e-08 ***
confint(m2)

##Por cada ug/huevo de concentraci?n de Herbicida que se suministra se espera un aumento 
#de la media del Indice de Da?o entre 0.031 y 0.045  con una confianza del 95%.

#Por cada unidad que aumente RU, el ID medio aumentara  0.03783, con una confianza del 95%.

#ID_i= 106.12053 +  0.03783  * RU_i     IDi~Normal(ui;sigma^2) i=1-18

###################################################################
###Grafico final con los predichos por el modelo de regresion######
library(ggeffects)
ggpredict(m2)
plot(ggpredict(m2), add.data = TRUE, grid = TRUE) 

###########################################################################
####Grafico final con estimaciones segun modelo de comparacion de medias###

emmeans(m1, pairwise ~ RU)
confint(comp) #magnitud del efecto

comp<-emmeans(m1, pairwise ~ RU)
estimaciones<-as.data.frame(comp$emmeans)
ggplot(estimaciones, aes(x=RU), y=emmean) + 
  labs(x="RU (ug/kg)") + labs(y="ID") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="darkblue", width=0.1)+ geom_point(shape=15, size=4, color="darkblue") + 
  ggtitle("Comparaci?n de volumen cerebral seg?n tratamiento", "Media (EE)")+
  annotate("text", x=c(1,2,3), y=c(2.25, 2.25, 2.25), label = c("A", "B", "C"))

ggpredict(m1)
plot(ggpredict(m1), add.data = TRUE, grid = TRUE) 


