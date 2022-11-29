# la siguiente sentencia para borrar la memoria:
rm(list=ls())

#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

class(Datos)
str(Datos)
dim(Datos)
head(Datos)
summary(Datos)

# Tipos de variables? # 
class(Datos$Riego)
Datos$Riego<- as.factor (Datos$Riego)
class(Datos$Fertilizacion)
Datos$Fertilizacion <- as.factor

#DESCRIPTIVA
# con CreateTableOne (libreria tableone)

tabla <- CreateTableOne(vars= "CRA",
                        strata = c("Fertilizacion", "Riego"),
                        data=Datos,
                        test = F)
tabla

# Graficos de perfiles (fundamental mirarlos para evaluar cualitativamente 
# si puede existir interacción entre las var predictoras)
# Si NO hubiese interaccion los perfiles deberian verse paralelos.

# tabla de medias de Fertilizacion*Riego
(medias.Datos<-aggregate(CRA~Fertilizacion+Riego, Datos,mean))

# Opcion grafica 1
gp <- ggplot(medias.Datos, aes(x=Fertilizacion, y=CRA, colour=Riego, group=Riego))
gp + geom_line(aes(linetype=Riego), size=.6) +geom_point(aes(shape=Riego), size=3) 

#COMENTARIO

################################
# Modelo e implementacion en R #
#------------------------------#
################################

modelo2 <- lm(CRA~Fertilizacion*Riego, data=Datos)

#############
# Supuestos #
#-----------#
#############


#Calculamos los residuos y los valores predichos por el modelo

e<-resid(modelo2) # residuos
re<-rstandard(modelo2) #residuos estandarizados
pre<-predict(modelo2) #predichos
res<-cbind(Datos$Fertilizacion, Datos$Riego, Datos$CRA,pre,e,round(re,3)) # cbind: "Combine R Objects by Columns"
colnames(res)<-c("Fertilizacion", "Riego", "CRA", "Predichos", "Residuos", "residuos std") # agregamos los "Column Names"
res

# View(res)
View(res)
#Supuestos

# Graficos diagnosticos
par(mfrow = c(1, 2))
# Residuos est vs valores predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)

#COMENTARIO

# qqplot 
qqPlot(e, main = "QQ Plot residuos")

#COMETARIO

# Pruebas estadisticas

# prueba de shapiro
shapiro.test(e) #p-value = 0.151 no rechazo normalidad
# Levene
leveneTest(CRA~Fertilizacion*Riego, Datos, center=mean) #p-valor:0.4762 no rechazo homocedasticidad

# test "global" para interaccion
anova(modelo2)

#H01: Alfa1 = 0 (no efecto del Fertilizante)
#H02: Bj= 0 (no efecto del Riego)
#H03: AlfaxBij = 0 (no hay interaccion entre las variables explicatorias)

##################
# Comparaciones  #
# -------------- #
##################

# Cuando tenemos interacciones involucradas podemos estar en alguno de los siguientes casos:
# 1) variables explicatorias están involucradas en una interaccion SIG
# 2) variables explicatorias no están involucradas en una interaccion SIG   

# Si ocurre 1) ->
#  Comparaciones de interaccion (todas las medias contra todas) 
#  o Efectos Simples (Fijar un nivel de uno de los factores y comparar entre los niveles del otro)

# Si ocurre 2) ->
#  Comparaciones de efectos principales

### Estamos en el caso 1) 
##    Dos posibilidad de enfoques analiticos

#       1.1   Comparaciones de interaccion (todas las medias contra todas) 

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

comp <- emmeans(modelo2, pairwise ~ densidad*provincia) # Tukey por default
comp


plot(comp$emmeans, comparisons = TRUE)

#COMENTARIO


#      1.2    Efectos simples
#             Dentro de cada nivel de uno de los factores comparo los niveles del otro

#       (*) Efecto simple A. Comparar los niveles de densidad dentro de cada nivel provincia

efsimple_densidad <- emmeans(modelo2, pairwise ~ densidad | provincia)
efsimple_densidad

plot(efsimple_densidad$emmeans, comparisons = TRUE)

#COMENTARIO

#       (*) Efecto simple B. Comparar entre provincias dentro de cada nivel de densidad

efsimple_provincia <- emmeans(modelo2, pairwise ~ provincia | densidad )
efsimple_provincia

plot(efsimple_provincia$emmeans, comparisons = TRUE)

#COMENTARIO

#       NOTA: solo realizar efectos simples en un sentido (opcion A o B, no ambas)
#              sino no se controla el error global

### Estamos en el caso 2) 
##  
#       
#COMPARACION DE EFECTO PRINCIPAL DE LA FERTILIZACION (SI HUBIESE DADO SIGNIFICATIVA)
options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

comp <- emmeans(modelo2, pairwise ~ Fertilizacion) # Tukey por default
comp


plot(comp$emmeans, comparisons = TRUE)

#COMENTARIO

#COMPARACION DE EFECTO PRINCIPAL DEL RIEGO

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

comp <- emmeans(modelo2, pairwise ~ Riego) # Tukey por default
comp


plot(comp$emmeans, comparisons = TRUE)

#COMENTARIO.

###################
# Grafico final y #
# conclusiones    #
# --------------  #
###################
#EFECTOS PPALES
library(ggeffects)
ggpredict(modelo2)
plot(ggpredict(modelo2), add.data = TRUE)

#COMENTARIO.

## Para concluir elegimos quedarnos con los resultados de EFECTOS SIEMPLES, opcion A
#  comparacion de los niveles de densidad dentro de cada nivel provincia


# Grafico (una de muchas opciones gráficas!)
# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
ggplot(resumen_modelo, aes(x=provincia, y=emmean, fill=densidad, coulor=densidad)) + 
  xlab("Procedencia del genotipo") +  ylab("concentración de aceite (ml de aceite/100gr de MS)") +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2, position=position_dodge(.9))+
  annotate("text", x = c("ba","mza","sl"), y=2, label = c("A               B", "A                B", "A               A"))

#comentario.

#OTRAS OPCIONES
#Gráfico con las estimaciones del modelo ####
estimaciones<-as.data.frame(comp1$emmeans)
ggplot(estimaciones) +
  aes(x = veg, y = emmean , color = fert, group = fert) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2)+
  geom_point(size=4) +
  geom_line(stat = "summary", fun = mean) + 
  labs(x="Planta") + labs(y="Hidrocarburos (g/100 g de suelo)") +
  theme(text=element_text(size=14))

#otra opción:
library(ggeffects)
estim<-ggpredict(m1, terms = c("veg", "fert"))
plot(estim, add.data = TRUE) 
