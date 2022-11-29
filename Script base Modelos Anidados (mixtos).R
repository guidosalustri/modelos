# la siguiente sentencia para borrar la memoria:
rm(list=ls())
setwd("")

#Escribir variables e hipotesis
#modelo condicional en formato comparacion de medias
# Horas_ijkl = mu + alfa_i + Beta_j + alfa* beta_ij + C_k(ij) + E_ijkl
# i = 1 a 2
# j = 1 a 2
# k(ij) = 1 a 3
# l(k) = 1 a 4

# E_ijkl ~ NID (0 ; varianza)
# C_k(ij) ~ NID (o; varianza_indiv)



# exploro la estructura del data.frame

Datos <- read.delim("HF.txt")

head(Datos)
str(Datos)
class(Datos$sexo)
class(Datos$Individuo)
class(Datos$Fumador)
class(Datos$horas.suenio)

Datos$sexo <- as.factor(Datos$sexo)
Datos$Individuo <- as.factor(Datos$Individuo)
Datos$Fumador <- as.factor(Datos$Fumador)

################################
#Descripcion de los datos. #
################################

# tabla con medidas resumen (dejar solo la importante a fines del trabajo)
#(horas de suenio promedio discriminadas por sexo y condicion de fumar)

library(dplyr)
Datos %>% 
  group_by(Fumador) %>%
  summarise(n = length (horas.suenio), 
            media = mean (horas.suenio), 
            DE = sd(horas.suenio),
            min = min(horas.suenio), 
            max = max(horas.suenio))


Datos %>% 
  group_by(sexo) %>%
  summarise(n = length (horas.suenio), 
            media = mean (horas.suenio), 
            DE = sd(horas.suenio),
            min = min(horas.suenio), 
            max = max(horas.suenio))

# Resumen dato x 'tratamiento'
a<-Datos %>% 
  group_by(sexo, Fumador, Individuo) %>% 
  summarize(horas.suenio = mean(horas.suenio)) 

# Resumen individuos x 'tratamiento'
b<-Datos %>%
  group_by(sexo, Fumador, Individuo) %>%
  summarise(n=n(),
            media1 = mean(horas.suenio)) %>% 
  summarize(n=n(),
            horas.suenio = mean(media1),
            DE=sd(media1)) 

# grafico donde se muestra la dispersion de los datos de horas de suenio  por sexo (eje x) 
#discriminadas por condicion de fumar (distinto color para fumador/no fumador)

library(ggplot2)
ggplot(Datos, aes(sexo, horas.suenio, color=Fumador)) +  geom_point() + labs(y="Horas de suenio")


# grafico donde, adem?s, se muestra las medias de cada uno de los 3 individuos en la condici?n 
# sexo+Habito de fumar 

ggplot(Datos, aes(sexo, horas.suenio, color=Individuo))+
  ylab("horas.suenio")+
  facet_grid(cols = vars(Fumador))+
  geom_jitter(size=2, alpha=0.3, position = position_jitter(width = .1))+  
  geom_jitter(data=a,  size=4, position = position_jitter(width = .1))+
  geom_point(data=b, aes(x=sexo, y=horas.suenio), color="darkblue", size=4)+
  geom_errorbar(data=b, aes(ymin = horas.suenio-DE, ymax = horas.suenio+DE), color="darkblue", width=0, size=1) #



##############################################
# Modelo y verificacion de los supuestos  #
##############################################


### Implemento Modelo Condicional (con 2 librer?as) y Modelo Marginal (son equivalentes)


# MODELO CONDICIONAL CON lme4 (m1)
library(lme4)
m1 <- lmer(horas.suenio ~ sexo*Fumador + (1|Individuo), Datos)


# MODELO CONDICIONAL CON nlme (m2)
library(nlme)
m2 <- lme(horas.suenio ~ sexo*Fumador, random= ~1 | Individuo, data = Datos)


# MODELO MARGINAL con gls  (m3)

library(nlme)
m3 <- gls(horas.suenio ~ sexo*Fumador, correlation=corCompSymm(form = ~ 1 | Individuo), data = Datos)

#No devuelve varianzas de individuos porque tiene matriz de correlacion que explicita la falta de 
#independencia en la observaciones.

#############################
# Supuestos (elijo m1) #
#############################

e  <-resid(m1) # 
pre<-predict(m1) #predichos
Ck <- ranef(m1)$Individuo$'(Intercept)'

par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gr?fico de dispersi?n de RE vs PRED",cex.main=.8 )
abline(0,0)

library(car)
qqPlot(e)
qqPlot(Ck)
par(mfrow = c(1, 1))

# Efectue la prueba de Shapiro para e y Ck

shapiro.test(e) #p-value = 0.8602
shapiro.test(Ck) #p-value = 0.9897

#Conclusion acerca de supuestos de normalidad y homocedasticidad de la VR.
#Conclusion acerca de la normalidad de la variable aleatoria.


#############################
# Analisis de Resultados y  # 
# Conclusiones  
# (vamos a trabajar con m1) #
#############################


# Parte Fija
# Significancia global
library(lmerTest) # agrega p-valores a la salida de lmer
m1 <- lmer(horas.suenio ~ sexo*Fumador + (1|Individuo),Datos)
anova(m1) # Anova(m1) paquete car (SC tipo III), drop1(m1) (seria analogo)

#interacci?n no significativa, es informativo a los fines de mi estudio.

# summary modelo 
summary(m1)

# Parte aleatoria

# Informar todas las varianzas estimadas (las saco del summary)

#varianza_indiv = 0.03637
#varianza_residual = 0.19389 

# Calcular e interpretar el CCI (a mano!, o se puede ver en la tabla resumen que aparece luego, tab_model)

# 84% es varianza no explicada
# 16% varianza explicada por individuos

# CONCLUSION DEL CCI
#Discriminar por individuos es necesario para cumplir los supuestos y acompa?ar el 
# dise?o pero no esta explicando gran parte de la variabilidad de los datos. 

# Significacion parte aleatoria
ranova(m1)

#no es significativo (p valor= 0.2838) el incluir la variable aleatoria indiv, no explica una 
#parte significativa de la varianza de la VR, pero igual no la puedo sacar, porque acompana mi disenio.

# Resultados con tab_model 
library (sjPlot)
tab_model(m1)

#CONCLUSION DE TABLA

# Predicciones 
fitted(m1)  # predicciones parte fija + aleatoria
fixef(m1)   # estimacion parametros efectos fijos
X <- model.matrix(m1)
pred_fija <- X %*% fixef(m1) # predicciones parte fija 

#Parte aleatoria
Ck<- ranef(m1)$Individuo$'(Intercept)' 
library(sjPlot)
library(sjlabelled)
library(sjmisc)
install.packages("glmmTMB") 
plot_model(m1, type="re", show.values = TRUE, value.offset = .3) #cuanto se aleja la varianza de cada individuo de la media.

# IC 95% para varianzas y coef de la parte fija.

#signif parte aleatoria
confint(m1, level = 0.95, method = c("profile"))
#podemos parametrizarlo como regresi?n, intervalos para el desvio estandar
summary (m1)


#Grafico final con estimaciones segun modelo
library(emmeans)

# En este caso como son 2 niveles de cada factor 
#  emmeans no es necesario para concluir sobre significancia
#  pero si podemos usarlo para IC, valores predichos, etc.


# Ingreso modelo completo para extraer predichos

comp1<-emmeans(m1, pairwise ~ sexo*Fumador)

medidas_resumen <- as.data.frame(comp1$emmeans)

ggplot(medidas_resumen, aes(x=Fumador, y=emmean, fill=sexo)) +
  geom_point(aes(colour=sexo)) + geom_line(aes(colour=sexo)) +
  labs(x="Fumador") + labs(y="horas de sue?o") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE, colour=sexo), width=0.2)+
  ggtitle("Comparaci?n de las horas de sue?o en funci?n del habito de fumar para sexo F/M", "Media ? SE")


#otra opci?n para medias predichas con ggeffects
library(ggeffects)
(a<-ggpredict(m1, 
              terms = c("sexo", "Fumador")))
plot(a, add.data = TRUE, grid = TRUE) 


#CONCLUSION DEL GRAFICO
#Valores predichos de horas de sue?o en funci?n del h?bito de Fumar (NO/SI) en hombres y 
#mujeres (media ? error est?ndar). A partir de un modelo mixto incluyendo como factor aleatorio al 
#individuo no se encontr? interacci?n entre el h?bito de fumar y el sexo (p>0.05) y se evidenciaron 
#diferencias en las horas de sue?o seg?n el h?bito de fumar (p<0.05).


