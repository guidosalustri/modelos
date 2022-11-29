rm(list=ls()) 

#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

Datos <- read.delim("BiomasaRatones.txt")
names(Datos)
head(Datos)
str(Datos)
View(Datos)
summary(Datos)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################

summary(Datos)

# library(pastecs)
round(stat.desc(Datos[2:5]), 2)

# para explorar asociaciones entre variables
plot(Datos)

# Correlation matrix
names(Datos)
corr_datos <- round(cor(Datos), 2)
print(corr_datos)

# library(ggplot2)
# library(ggcorrplot)
# library(corrplot) 


# Asociaciones entre variables
# Diferentes opciones graficas

install.packages("GGally")
library(GGally)
ggpairs(Datos)

ggcorrplot(corr_datos,  
           type = "lower", 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           outline.color="black",
           ggtheme=theme_bw)

ggcorrplot(corr_datos,  
           type = "lower", 
           method="square", 
           outline.color="black",
           ggtheme=theme_bw)

# con otra libreria 
corrplot(corr_datos, 
         type = "upper",
         method = "number")

# mixto
corrplot.mixed(corr_datos, 
               lower.col = "black", 
               number.cex = .7)

# ¿Qué variables se encuentran asociadas?

# m1
# modelo aditivo completo 
m1 <- lm(ratones ~ lluvia+predadores+cobertura+semillas, Datos)

# factor de inflacion de la varianza (VIF)
# library(car)
vif(m1) #Valores superiores a 5 indican colinealidad importante

# comienza la "Backward selection" (tambien podria ir sumando variables de a una)
#Voy probando distintos modelo sacando variables y probando interaccion.
#HAGO MODELO, PRUEBO DROP, COMPARO CON OTROS MODELOS CON ANOVA. DESPUES COMPARO CON AIC Y R AJUSTADO

# Existen varios criterios distintos (entre otros):

# Identificar los terminos NS en el summary. Que variable eliminarian con este criterio?
#  Comparar a traves de cambios en el AIC . 
#  Una forma de ver esto es con la funcion drop1, que compara el modelo con y sin
# "X" termino. Además esta funcion drop1 devuelve la Suma de cuadrados residual del modelo.

drop1(m1)

# prueba de hipotesis para modelos anidados
# determina la significacion de la reducción en la SC residual

anova(m1,m2)
drop1(m1, test="F") # y tambien se puede ver con drop1, agregando el test 

# La inclusion de una interacción suele inducir colinealidad, 
#   solo afecta los EE de los términos de menor orden 
#Los VIF son muy grandes

# CMe 
CMe <- round(c(summary(m1)$sigma^2,summary(m2)$sigma^2,summary(m3)$sigma^2,summary(m4)$sigma^2,summary(m5)$sigma^2), 2)

# R2 (no para comparar entre modelos)
R2 <- c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared, summary(m5)$r.squared)

# R2 ajustado
R2aj <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared)

#AIC
AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5))

# Nombre modelo (para generar un data frame bonito)
modelo <- c(1,2,3,4,5)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp

####################
# Validacion cruzada
####################

# LOO CV
# library(caret)

# comparo mis cuatro modelos candidatos
# agrego un m5, muy simple

# Indicamos la función para el entrenamiento 
train.control<-trainControl(method = "LOOCV") 

# Entrenamos (estimamos) el modelo  (n modelos con n-1 observaciones) 

m1loo <- train(ratones ~ lluvia+predadores+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m2loo <- train(ratones ~ lluvia+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m3loo <- train(ratones ~ lluvia+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m4loo <- train(ratones ~ lluvia*semillas, 
               data=Datos, method ="lm",trControl= train.control)
m5loo <- train(ratones ~ lluvia, 
               data=Datos, method ="lm",trControl= train.control)

# resultados

print(m1loo)
print(m2loo)
print(m3loo)
print(m4loo)
print(m5loo)

#dataframe con los resultados
b<-m1loo$results
c<-m2loo$results
d<-m3loo$results
e<-m4loo$results
f<-m5loo$results

comparacion_modelos <-rbind(b,c,d,e,f)
comparacion_modelos <- comparacion_modelos[,2:4] 

#agrego error relativo
comparacion_modelos$ER=comparacion_modelos$RMSE/mean(Datos$ratones)*100  
comparacion_modelos

###########################################################
# Modelo(s) seleccionado(s): Resultados e interpretacion  #
#-------------------------------------------------------  #
###########################################################

# Analicemos m3
summary(m3)

# chequeamos VIF
vif(m3) #no deben ser mayor a 5

# Ecuacion estimada del modelo


# concluir en terminos de la pregunta de investigacion


# Interpretacion de coeficientes (magnitud de efecto) 
#Ejemplo coef lluvia: por cada mm adicional se espera un aumento en el promedio de la 
#biomasa de ratones desde 0.69 kg hasta 1.90 kg manteniendo constante la disponibilidad 
#de semillas (para disponibilidad de semillas de entre 3 y 9 kg/anual) con una confianza del 95%

########################
# Grafico/s final/es   #
# -------------------  #
########################

#Visualizando resultados
#grafico 3d modelo final (2VE)
library(car)
install.packages("rgl")
library(rgl)
scatter3d(VVM~ edad+altura, fill=FALSE, data=bd)
confint(m3)

# Efecto parcial de cada variable. Predicciones marginales 
# ajusta a la media ponderada de la otra variable
library(ggeffects)


## opcion con ggpredict
#  si se explora el objeto "a" se puede ver a que valor de la otra variable se 
#   esta graficando

(a<-ggpredict(m3))
a
plot(a, add.data = TRUE, grid=TRUE)


## opcion plot model
library(sjPlot)
# efectos principales:
plot_model(m3, type = "pred", terms = c("altura"))
plot_model(m3, type = "pred", terms = c("edad"))
#ef simples, seleccionado algunos valores de la otra VE 
plot_model(m3, type = "pred", terms = c("altura", "edad[20, 50, 80]"))
plot_model(m3, type = "pred", terms = c("edad", "altura[150, 170, 190]"))

# estimacion y comparación de la magnitud del efecto

tab_model(m3) # sjPlot

# puede chequear que son los IC
confint(m3)


# install.packages("lm.beta")
#   library(lm.beta)
lm.beta(m3) #coeficientes estandarizados


# Validación ####
plot(predict(m3),bd$VVM, ylab="Observados", xlab="Predichos")
abline(0,1, col="red")
cor <- cor(predict(m2), Datos$PS)
cor 
cor^2

# Predicciones para nuevas observaciones ####
nuevo = data.frame(edad=30, altura =170)
predict(m3, nuevo) 
#con IC
predict(m3, nuevo, interval="prediction", level=0.95)

###########################
# Seleccion automatica
###########################

# library(MuMIn)
# modelo aditivo
dredge(lm(ratones ~ lluvia+predadores+cobertura+semillas, data = Datos, na.action = "na.fail"))

# modelo con interaccion
dredge(lm(ratones ~ lluvia*predadores*semillas, data = Datos, na.action = "na.fail"))
