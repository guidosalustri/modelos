rm(list=ls())

datos <- read.delim("Ipomopsis.txt")
View(datos)
head(datos)
str(datos)
names(datos)
class(datos$raiz)
class(datos$pastoreo)
datos$pastoreo <- as.factor(datos$pastoreo)

#Grafico de dispersion
disp <-ggplot(datos, aes(x =raiz , y = peso_sem, colour =pastoreo)) + geom_point(size=2)
disp <- disp + xlab("tamanio de la raiz (cm)") +  ylab("peso de la semilla (mg)") +  ggtitle("DBO vs pH y fuente")
disp

#Predicciones, el peso de la semilla aumenta en condiciones de pastoreo, sin embargo, como se observa
#que plantas con raiza de mayor tamnio tienen semillas de peso mayor, podria ser que estas dos 
#variables explicativas esten interaccionando, y no se le pueda atribuir un mayor peso de semilla
#unicamente a la condicion de pastoreo. VER PRUEBA ESTADISTICA DE INTERACCION.

#Modelo
# P_sem_i= B0 + B1*raiz + B2*pastoreo + B3*raiz*pastoreo + E_i (E_i ~ NID (0, var)) i=1-40.
#ademas de los supuestos de linealidad, normalidad y homocedasticidad, estan los de disenio (indep en obs).

m1 <- lm(peso_sem ~ raiz*pastoreo, datos)

#supuestos
e<-resid(m1) # residuos
re <- rstandard(m1) 
pre<-predict(m1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0)
library(car)
qqPlot(e, main = "QQplot -Modelo 1")
shapiro.test(e) #p-value = 0.559 No hay evidencias para rechazar normalidad.
#Mas alla de algun dato medio atipico no se ven patrones de heterocedaticidad.(no puedo hacer levene, no replicas)

#Ahora si busco pruebo significancia de la interaccion 
summary(m1) #pvalor: 0.7500 es NO significativo.

# Yi^= -125.173 + 23.240*raiz + 30.806*sin_pastoreo + 0.756*raiz*sin_pastoreo

#Planteo un modelo sin interaccion que sera mas parsimonioso porque estima un menor numero de parametros
#En este caso puedo sacar el termino de la interaccion debido a que no se tenia ninguna hipotesis 
#previa acerca de la misma entre el diametro de la raiz y el pastoreo, si la hubiese tenido, 
#deberia dejarla aunque fue NS ya que es informativo. 
#Ademas, el diametro de la raiz no es un tratamiento asigando sino que es una variable observacional.

m2 <- lm(peso_sem ~ raiz+pastoreo, datos)

#supuestos
e<-resid(m2) # residuos
re <- rstandard(m2) 
pre<-predict(m2) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0)
library(car)
qqPlot(e, main = "QQplot -Modelo 1")
shapiro.test(e) #No hay evidencias para rechazar normalidad (p-value = 0.4637)
#Mas alla de algun valor cuyos residuos son mayores a 2 o -2, no hay grandes patrones que senialen heterocedasticidad.

summary(m2)
#Tanto el diametro de la raiz como el pastoreo tienen efecto significativo sobre el peso de la semilla.
# Yi^= -12.829 + 23.560*raiz + 36.103*sin_pastoreo 

#Este modelo no explica el 8% de la variabilidad en el peso de la semilla.

#El efecto de la herbivoría una vez descontado el efecto que tiene el tamaño de la 
#planta sobre la producción de frutos medio es de 36,103 mg por cada unidad sin pastoreo.

#  Seleccione un modelo a presentar e Informe los resultados obtenidos 

R2 <- c(summary(m1)$r.squared, summary(m2)$r.squared)
AIC <- c(AIC(m1), AIC(m2))
R2aj <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared)
CMe <- round(c(summary(m1)$sigma^2,summary(m2)$sigma^2))
modelo <- c(1,2)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp
#Me quedo con el dos que no incluye la interaccion y es mas parsimonioso
# con IC
library(ggeffects)
library(dplyr)
ggpredict(m2, 
          terms = c("raiz", "pastoreo"),
          interval = "confidence")   %>% plot(add.data=T)
confint(m2)


#Por otro lado, podemos ver que los predichos para
# el peso de las semillas para un mismo diámetro de raíz (controlado x raiz) son entre  29.30052  y 42.90598 mg mayores en condición 
#de ausencia de pastoreo respecto a en presencia del mismo (respecto a la referencia) (con una confianza del 95%), sin embargo, se alcanzan
#pesos mayores de semillas en condiciones de pastoreo (lo cual se puede asociar a un proceso de sobrecompensacion).
#El aumento en un cm del diametro de la raiz genera un aumento del peso de la semilla de entre 21.23242 y 25.88768 mg en promedio (con una confianza del 95%).
#El diametro de la raiz se uso para controlar la variabilidad, no forma parte de la pregunta de investigacion.

#No planteo un modelo sin diametro de raiz porque dio significativo,
# Nos permite explicar parte de la variabilidad de la VR, eso 
# mejora la potencia de nuestra prueba aunque la variable no sea de interes para la investigacion.

