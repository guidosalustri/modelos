
rm(list=ls())
#Poner las VR y VE, disenio etc etc.

setwd("C:/Users/...")

#Plantear modelo con sus subindices y supuestos.

#gr?fico de perfiles para estudiar el paralelismo entre bloques

#En bloque es deseable que los bloques respondan al tratamiento de forma paralela (no interaccion)
#Es decir, que no haya un efecto diferencial del tratamiento sobre los bloques. 
# Se consideraria (si tuviese n grande) un modelo de pendiente aleatoria donde se estima una pendiente para cada camada. 
#Esto no seria util, para el objetivo del problema debido a que las conclusiones que te obtienen 
#son esfecificas para las camadas muestreadas y las inferencias no se podrian extrapolar a la poblacion de camadas.

Datos  <- read.delim("Hormona.txt")

#VER CLASE DE VARIABLES
#Pasamos el bloque ("camada") a variable cualitativa 
Datos$Camada <- factor(Datos$Camada)

# ordenar los niveles (Testigo-Baja-Alta)
levels(Datos$Hormona)
Datos$Hormona<-factor(Datos$Hormona,levels=c("Testigo","Baja", "Alta"))
levels(Datos$Hormona)

library(ggplot2)
ggplot(Datos, aes(Hormona, GanPeso, color=Camada, group= Camada)) + 
  geom_point() + 
  labs(x="Dosis Hormona", y="Ganancia de peso (dg)") + 
  geom_line()

#CONCLUSION SOBRE PARALELISMO.

# Descriptiva ####
library(dplyr) #para poder hacer descriptiva
Datos %>% 
  group_by(Hormona) %>%
  summarise(n = length (GanPeso), 
            media = mean (GanPeso), 
            DE = sd(GanPeso),
            min = min(GanPeso), 
            max = max(GanPeso)) #La ganancia de peso medio es menor para testigo que para dosis baja y alta de hormona.

Datos %>% 
  group_by(Camada) %>%
  summarise(n = length (GanPeso), 
            media = mean (GanPeso), 
            DE = sd(GanPeso),
            min = min(GanPeso), 
            max = max(GanPeso)) #Las camadas muestran distintas medias, razon por la cual se hicieron los bloques (controlar varianza entre camadas)

library(ggcorrplot) #hacer boxplot (un grafico un poco mas informativo) 
qplot(Hormona, GanPeso, data = Datos, 
      geom=c("boxplot","jitter"), fill=Hormona) 

#COMENTARIO

#Modelo y verificacion de los supuestos 
#considerando camadas como de efectos aleatorios (modelo condicional) usando lme4

library(lme4)
m1 <- lmer(GanPeso ~ Hormona + (1 | Camada), data = Datos)
summary(m1)

#para ver la significaci?n:
install.packages("lmerTest")
library(lmerTest)
m1 <- lmer(GanPeso ~ Hormona + (1 | Camada), data = Datos)
summary(m1)

# Analizar los Supuestos
e<-resid(m2) # son los residuos de Pearson 
pre<-predict(m2) #predichos
Bj <-ranef(m2)$Camada$'(Intercept)'
par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gr?fico de dispersi?n de RE vs PRED",cex.main=.8 )
abline(0,0)
qqnorm(e, cex.main=.8)
qqline(e)
qqnorm(Bj, cex.main=.8)
qqline(Bj)
par(mfrow = c(1, 1))
shapiro.test(e)
shapiro.test(Bj)

#Comentar resultados de supuestos de normalidad y homocedasticidad de la VR
#Comentar resultados de supuestos de normalidad de la variable aleatoria.

# Analizar la significancia de la parte fija
#H0: todo los alfa_i son iguales a cero. La ganancia de peso media no difiere para los niveles de hmorona.
#H1: algun alfa_1 difiere de cero. La ganancia de peso media difiere para alguno de los niveles de hormona

summary(m1)
anova(m1)
drop1(m2)
m0<- lmer(GanPeso ~  (1|Camada), data=Datos)
anova(m0,m2)
      
confint(m1)

#Comentario: ver la significancia en el anova, que infor obtengo? concluyo (p valor)

#comparaciones pertinentes
library (emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

comp <- emmeans(m1, pairwise ~ Hormona) # Tukey por default
comp
conint(comp)
plot(comp$emmeans, comparisons = TRUE)

#Conclusion en base a los p valor de los contrastes y SIEMPRE informando la magnitud de efecto 
#con los estimados o los IC. #confint(comp)

#significancia de la parte aleatoria (ranova) y componentes de varianza

ranova(m1) 
#COMENTARIO
#p-valor:1.44e-05 *** Dado que da significativa la prueba, implica que la camada explica variabilidad de la variable respuesta. 
#Incluirla disminuye la varianza residual, no explicada por el modelo. Sin embargo, si no fuese significativa, 
#igualmente incluiriamos esta variable, dado que el modelo debe acompanar el diseno experimental.

# #Random effects: (INCLUIR ESTA SALIDA PONIENDO CUANTO EXPLICA DE LA VARIABILIDAD DE LA VR LAS CAMADAS, ICC, puedo ver en tabla resumen)
#   Groups   Name        Variance Std.Dev.
# Camada   (Intercept) 1.5347   1.2388       La varianza entre camadas (debida a las camadas)
# Residual             0.1532   0.3914       La varianza residual no explicada por el modelo.

ranef(m1) #alfa i (BLUP) de cada camada, graficado
library(sjPlot)
plot_model(m1, type = "re")

## Grafico de componentes de varianza
# Simple Pie Chart
slices <- c(1.53, 0.15) #Aca pongo la varianza de cada una.
lbls <- c("Camadas", "Residual")
pie(slices, labels = lbls, main="Variance components")
#El 90.9 % de la variaci?n en la ganancia en peso est? explicada por las camadas.

#### Predicciones sujeto-especificas: parte fija + aleatoria ######

coef(m1) #muestra los coef para cada nivel aleatorio (camada)

fitted(m1)  #predicciones parte fija + aleatoria
fixef(m1) #estimacion coeficientes de los efectos fijos (mismos del summary)
X<-model.matrix(m1)
pred_fija<-X %*% fixef(m1)#predicciones parte fija 
alfai<-rep(round(ranef(m1)$Camada$'(Intercept)', 4),each=3)
pred<-cbind(Datos$Hormona,Datos$Camada, Datos$GanPeso,pred_fija, alfai,round(fitted(m2),4))
colnames(pred)<-c("Hormona", "camada","GanPes", "pred fija", "efecto aleat", "pred_fija_mas_aleat")
pred



# Presente un Grafico de dispersion del modelo estimado 
library(ggeffects)
(a<-ggpredict(m1, 
              terms = c("Hormona")))
plot(a, add.data = F, grid = TRUE) # sin datos superpuestos
+ labs(x=, y=, title= "")

#Parte fija + aleatoria ####
# Grafico de dispersion con camadas y modelo estimado

pred <- as.data.frame(pred)
library(ggplot2)
p <- ggplot(pred, aes(Hormona, pred_fija_mas_aleat, colour=factor(camada)))
p + geom_point() +labs(x="Hormona", y="Ganancia en peso predicha (dgr)")+ geom_line()

# Tabla Resumen del modelo
install.packages("sjPlot")
library(sjPlot)
tab_model(m1)

#COMENTARIO

# MODELO MARGINAL 
#   Plantee aqui la alternativa MDOELO MARGINAL con la libreria gls
library(nlme)
m2c <- gls(GanPeso ~ Hormona, correlation=corCompSymm(form = ~ 1 | Camada), data = Datos)
summary(m2c)
#No descompone la varianza, residual standar error tiene toda la varianza junta. Residual standard error: 1.299173 
#Estima un rho, que utiliza en la matriz de covarianza.
#El resultado para el factor fijo es el mismo.


