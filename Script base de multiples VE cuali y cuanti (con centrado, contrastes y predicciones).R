rm(list=ls())
#ANALISIS EXPLORATORIO
DBO_vs_pHyFue <-ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=2)
DBO_vs_pHyFue <- DBO_vs_pHyFue + xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH y fuente")
DBO_vs_pHyFue
#Primero planteo el modelo completo con interaccion, supuestos, y anova
#Ecuaciones estimadas para el modelo completo
#GRAFICO DE PERFILES DONDE SSE VE INTERACCION

p1 <- ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=2)
q1 <- p1 + xlab("pH") +  ylab("DBO") +  ggtitle("DBO en función del pH y la fuente")
r1 <- q1 + geom_smooth(method = "lm", se = FALSE)
r1

#DEL MODELO CON INTERACCION VOY COSNTRUYENDO LAS DISTINTAS ECUACIONES SEGUN DE QUE NIVEL DE LA CUALI HABLE
#### Fuente A

# y^= b0+b1*pH

# y^ = 39.4 + 40.26*pH

### Fuente B

# y^ = 39.4 -306.4 + (40.26 + 30.95)*pH
# y^ = -267 + 71.21*pH

### Fuente C

# y^ = 39.4 -197.639 + (40.26 + 13.56)*pH
# y^ = -158.24 + 53.82*pH

#VOY PROBANDO LOS DISTINTOS MODELOS CON SUS SUPUESTOS (SIEMPRE QUE TENGA SENTIDO EN EL CONTEXTO DEL PROBLEMA)

###############################
# Comparacion de modelos
###############################
# R2 (no para comparar entre modelos)
R2 <- c(summary(modelo1)$r.squared, summary(modelo2)$r.squared, summary(modelo3)$r.squared)

# R2 ajustado
R2aj <- c(summary(modelo1)$adj.r.squared, summary(modelo2)$adj.r.squared, summary(modelo3)$adj.r.squared)

#AIC
AIC <- c(AIC(modelo1), AIC(modelo2), AIC(modelo3))

# CMe 
CMe <- round(c(summary(modelo1)$sigma^2,summary(modelo2)$sigma^2,summary(modelo3)$sigma^2),2)

# Nombre modelo (para generar un data frame bonito)
modelo <- c(1,2,3)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp

#TAMBIEN PUEDO HACER PRUEBA DE HIPOTESIS ENTRE MODELOS ANIDADOS
anova(modelo2, modelo1)
drop1(modelo1, test = "F")

#####################################################
# Modelo seleccionado: Resultados e interpretacion  #
#-------------------------------------------------  #
#####################################################
# significancia de los terminos del modelo
anova(modelo1)  

# coeficientes estimados 
# escriba a partir del summary las ecuaciones estimadas para las 3 fuentes
summary(modelo1)

#Conclusiones en funcion de los estimados del summary, viendo como se modifica para cada fuente.
#Dar la magnitud de efecto, siempre sobre la referencia.

# IC 95% para los coef
confint(modelo1)


##################
# Comparaciones  #
# -------------- # PARA DISTINTAS OPCIONES, ENTRE NIVELES DEL FACTOR CUALI (fijando la cuanti)
##################

# seteo de opciones de salida del emmeans

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),contrast = list(infer = c(TRUE, TRUE))))

# Comparacion de pendientes

comp_pendientes <- emtrends(modelo1, ~ fuente, var="pH") #, contr="cld"
comp_pendientes
pairs(comp_pendientes)
plot(comp_pendientes, comparisons = TRUE)

# unicamente hay diferencias significativas entre fuente a y b , fuente c se superpone con a y b
#magnitud de efecto con los estimados o los IC

# Comparacion entre fuentes, ¿a que valor de pH?

Fuentes_en_PH_promedio <- emmeans(modelo1, pairwise ~ fuente|pH)
Fuentes_en_PH_promedio # dif entre fuentes para pH promedio (ver que lo informa la salida)
plot(Fuentes_en_PH_promedio$emmeans, comparisons = TRUE)

#todas las fuentes son significativamente diferentes (dar magnitud de efecto)
#independizandonos del pH vemos que las tres fuentes estan diferentemente podridas (diferentes valor de DBO)

## Si quiero comparar en el min y max de PH: 
Fuentes_en_PH_PH_min_max<-emmeans(modelo1, pairwise~ fuente:pH, cov.reduce = range)
Fuentes_en_PH_PH_min_max

########################
# Grafico final y      #
# conclusiones         #

# con IC
library(ggeffects)
library(dplyr)
ggpredict(modelo1, 
          terms = c("pH", "fuente"),
          interval = "confidence")   %>% plot(add.data=T)

#OTRO GRAFICO FINAL CON LAS RECTAS PUESTAS A MANO *ARTESANAL* Calculo segun recta original para 
#cada nivel de la VE cualitativa (pongo los estimados por el modelo sacado del summary)

#Modelo 2: con 2 VE sin interaccion ##### 
ggplot(bd, aes(x =edad , y = vvm, colour =sexo)) +
  geom_point(size=2) + 
  xlab("Edad (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad y el sexo") + 
  geom_abline(intercept = 155.85, slope = -0.91, colour="red") +
  geom_abline(intercept = 188.06, slope = -0.91, colour="skyblue")

#Modelo 3: con 2 VE con interaccion ####
ggplot(bd, aes(x =edad , y = vvm, colour =sexo)) +
  geom_point(size=2) + 
  xlab("Edad (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad y el sexo") + 
  geom_smooth(method = "lm", se = FALSE

########################
# Validación modelo 1  #
# -------------------  #
########################

# Predichos vs observados
p<-ggplot(Aguas, aes(x =Y , y = predict(modelo1), colour =fuente)) + geom_point(size=4)
p + geom_abline(intercept = 0, slope =1) +  ggtitle("Predichos vs observados")
cor <- cor(predict(modelo1), Aguas$Y)
cor
cor^2

#CORRELACION ALTA SIGNIFICA QUE LOS PREDICHOS POR EL MODELO SON MUY PARECIDOS A LOS OBS.
#ALTA CAPACIDAD EXPLICATIVA DEL MODELO.


################
# Predicciones #
# ------------ #
################


####  prediccion de Y para nuevos valores de x  ######
####              IC                            ######

nuevo = data.frame(fuente= "B", pH=7)
nuevo # exploren que es el objeto "nuevo"
predict(modelo1, nuevo, interval="confidence") 


####  prediccion de Y para nuevos valores de x  ######
####                IP                          ###### 

nuevo = data.frame(fuente= "A", pH=7)
nuevo # exploren que es el objeto "nuevo"
predict(modelo1, nuevo, interval="predict") 

# observar que el intervalo de prediccion para una observacion individual es mas amplio 
# que la banda de confianza del modelo para la media poblacional

###############
# Modelo      #
# centrado    #
# ----------  #

#Centrando en el valor de pH promedio
mean(Aguas$pH) # 
Aguas$pH_c <- Aguas$pH - mean(Aguas$pH)

modelo4<-lm(Y ~ pH_c*fuente, Aguas) #sigue todo igual
