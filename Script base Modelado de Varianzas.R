################################
### Modelado de varianzas.     #
#   ---------------------      #
################################

rm(list=ls())

#Relacion media varianza
ggplot(estad, aes(x=media, y=var))+geom_point(size=4) +   xlab("Media") +  ylab("Varianza") +  ggtitle("Relación media - varianza") +   
  geom_smooth(method="lm", se = FALSE, color="red", linetype="dashed")

###OTRA OPCION###

# Calcular medias y varianzas, y graficar para ver la relacion.
# con plot()
media <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, mean))
varianza <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, var))

tratamiento <- c("CC","SAC30", "SAC68") # Para colocar etiquetas.

plot(x = media,
     y = varianza,
     main = "Relacion varianza - media",
     ylab = "varianza",
     col = "red")

text(media, varianza, labels = tratamiento, cex = 0.7, pos = 2)

##### Otra opcion grafica ####

# Con ggplot (armo previamente un data frame con medias-varianzas)
DF <- as.data.frame(media)       # DF es el nombre del data.frame. Agrego la media
DF$sd <- round(sd,1)             # Agrego sd a DF, y redondeo a 1 decimal
DF$varianza <- round(varianza,1) # Agrego varianza a DF, yredondeo a 1 decimal
DF                               # Exploro el data.frame
# Genero y agrago la columna Tratamiento (en el orden que corresponde)
Tratamiento <- c("CC", "SAC30", "SAC68")  
DF$Tratamiento <- Tratamiento
DF                               # Exploro el data.frame

ggplot(DF, aes(x=media, y=varianza)) + 
  theme_bw() +
  geom_point(size=6, shape=1) +
  annotate("text", x=70,  y=2000,   label= "SAC30", size=6) +
  annotate("text", x=350, y=21000, label= "SAC68", size=6) +
  annotate("text", x=400, y=18000, label= "CC", size=6) 


## Modelos gls() modelando varianza

## ################################
## Modelo 3: VarIdent(Tratamiento)#
###################################    

library(nlme)
modelo_varIdent <- gls(T_ing ~ Tratamiento,
                       weights = varIdent(form = ~1 | Tratamiento),
                       data = Datos)

# Evaluar supuestos.

# Calcular de los residuos de pearson.
# Calcular los valores predichos por el modelo.

r3 <- residuals(modelo_varIdent, type="pearson") # = estandarizados.
pred3 <- fitted(modelo_varIdent)

plot(x = pred3,
     y = r3,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un boxplot de los residuos del modelo

boxplot(r3 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

# Graficar un qqplot.

qqPlot(r3, main = "QQ Plot residuos estandarizados")

# Prueba de Levene.

leveneTest(r3, Datos$Tratamiento, center = "median")


## ################################
## Modelo 4: "varPower"           #
###################################    

modelo_varPower <- gls(T_ing ~ Tratamiento,
                       weights = varPower(),
                       data = Datos)

# Supuestos.

r4 <- residuals(modelo_varPower, type = "pearson")
pred4 <- fitted(modelo_varPower)
plot(x = pred4,
     y = r4,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0)

boxplot(r4 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r4, main = "QQ Plot residuos estandarizados")

leveneTest(r4, Datos$Tratamiento, center = "median")



###################################
##  Modelo 5: "varExp".           #
###################################    

modelo_varExp <- gls(T_ing ~ Tratamiento,
                     weights = varExp(),
                     data = Datos)

# Supuestos.

r5 <- residuals(modelo_varExp, type = "pearson")
pred5 <- fitted(modelo_varExp)

plot(x = pred5,
     y = r5,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0)

boxplot(r5 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r5, main = "QQ Plot residuos estandarizados")

leveneTest(r5, Datos$Tratamiento, center = "median")




###################################
##  Seleccion de modelos.         #
###################################  

AIC(modelo_varIdent, modelo_varPower, modelo_varExp)

# Decidir cual es el mejor.


######################################
## Interpretacion del modelo elegido.#  
######################################  


modelo_varPower
summary(modelo_varPower)
anova(modelo_varPower)

# Indique si se rechaza la H0 del AnOVa.
# Interprete los parametros del modelado de varianza.

# Comparaciones a posteriori.

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


multCompTukey <- emmeans(modelo_varPower, pairwise ~ Tratamiento)

multCompTukey


plot(multCompTukey$emmeans, comparisons = TRUE)

# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(multCompTukey$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=Tratamiento, y=emmean)) +
  labs(x="Tratamiento") + labs(y="Tiempo ingesta [seg]") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ggtitle("Comparaciones", "Media ± Error estándar") +
  annotate("text", 
           x = c("CC","SAC30","SAC68"), 
           y = c(650,650,650), 
           label = c("B", "A", "B"))

