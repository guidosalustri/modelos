rm(list = ls()) 

library(ggplot2)  # Lo utilizaremos para realizar algunos graficos.
library(reshape2) # Utilizaremos la funcion dcast() para pasar de formato long a wide.
library(nlme)     # Utilizaremos la funcion gls(), que permite el modelado de varianza y tambien estructuras de correlacion.
library(corrplot)
library(emmeans)
install.packages("margins")
library(margins)
library(car)

datos <- read.delim("Suenio.txt")
names(datos)
str(datos)
summary(datos)
View(datos)
datos$individuo <- factor(datos$individuo)
class(datos$individuo)

#reordenar los niveles de los factores para que los niveles que vayan al intercept sean los que 
#deseamos. Para esto tambien utilizaremos la funcion factor(). 
# Utilizamos el argumento 'levels' de la funcion factor() para reordenar los niveles.

datos$tratamiento <- factor(datos$tratamiento,
                            levels = c("placebo", "cafeina"))

datos$tiempo <- factor(datos$tiempo,
                       levels = c("inicial","dia 1", "dia 2", "dia 3"))

### Graficos descriptivos.

## Box-plot.

boxplot <- ggplot(datos, aes(x = tratamiento, y = t_reac, color=tiempo)) +
  geom_boxplot(aes(color=tratamiento), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(shape=tiempo), position = position_jitter(width = .2))+theme(legend.position="right", legend.text=element_text(size = 14),legend.title = element_text(size=12, face="bold")) +
  ylab("T de reaccion")+xlab("Tratamiento")

boxplot

## Spaghetti-plots.

# Los spaghetti-plots permiten observar el comportamiento de una de las variables a
# lo largo de las observaciones, para cada individuo.
ggplot(data = datos, aes(x = tiempo,
                         y = t_reac,
                         colour = factor(individuo),
                         group = individuo)) +
  labs(title = "Spaghetti-plot",
       x = "Tiempo",
       y = "Tiempo de reaccion") +
  geom_point() +
  geom_line() +
  facet_grid(. ~tratamiento ) +
  theme_bw()

#Pareceria que la magnitud de efecto del tiempo sobre el tiempo de reaccion es mayor para el tratamiento 
#placebo que para el tratamiento cafeina.

## Evolucion de los distintos tratamientos en el tiempo.

# Generamos un grafico que une con lineas las medias de cada 'tiempo', con el color dependiendo del 
#tratamiento.

#######GRAFICO DE PERFILES#######

ggplot(data = datos, aes(x = tiempo,
                         y = t_reac, 
                         fill = tratamiento)) +
  
  geom_smooth(aes(group = tratamiento, colour = tratamiento), se = FALSE) +
  
  labs(title = "Grafico de perfiles considerando tratamiento y tiempo",
       x = "Tiempo",
       y = "Tiempo de reaccion") +
  
  theme_bw()

### Para estudiar las correlaciones entre tiempos.

##PARENTESIS.
# Vamos a estudiar la correlacion entre los datos tomados a distintos tiempos. 
#Para hacer esto deberemos utilizar la funcion dcast() del paquete 'reshape2'.

# El argumento 'formula' especifica que una individuo y tratamiento en funcion de la 
#variable tiempo. 'value.var' especifica el nombre de la columna de la cual se toman 
#los valores de la variable respuesta.

datos_wide <- dcast(datos,
                    formula = individuo + tratamiento ~ tiempo,
                    value.var = "t_reac")
datos_wide #Nos pasa la variable tiempo a distintas columnas segun sus niveles para hacer la matriz de covarianza.

# Nos vamos a quedar con las columnas que tienen informacion sobre el tiempo de
# respuesta para facilitar el calculo de las correlaciones.

datos_correlaciones <- datos_wide[,3:6]

# Grafiquemos para buscar relaciones entre las nuevas variables.

plot(datos_correlaciones)

library(GGally)
ggpairs(datos_wide, columns=3:6) #Esta funcion grafica con los coef de correlacion directamente.


# coeficientes de correlacion
coef_corr <- round(cor(datos_correlaciones), 2)
coef_corr

#De forma grafica (mas facil de visualizar)
library(corrplot)
corrplot(coef_corr, 
         type = "upper",
         method = "number")

#Dias contiguos tienen correlacion mayor que dias no contiguos. El inicial tiene la menor correlacion
#con todos los dias, no hay efecto del tratamiento.

#matriz de covarianza
round(cov(datos_correlaciones), digits = 2)

#Nueva base de datos utilizando la funcion melt() del paquete 'reshape2'
#en el dataframe en formato wide para tener al tiempo inicial como una covariable porque el tiempo
#inicial no deberia diferir entre niveles del tratamiento y ademas considerarlo como un nivel mas
#induciria una interaccion entre tiempo*tratamiento.
#Considero entonces al tiempo inicial como una covariable para la respuesta de cada individuo.

datos_con_inicial <- melt(datos_wide,
                          id.vars = c("individuo", "tratamiento", "inicial"),
                          variable.name = "tiempo",
                          value.name = "t_reac")
datos_con_inicial

head(datos_con_inicial)
summary(datos_con_inicial)

# Al sacar el t_inicial del factor tiempo, reviso como quedaron las correlaciones 
datos_wide <- dcast(datos_con_inicial,
                    formula = individuo + tratamiento ~ tiempo,
                    value.var = "t_reac")
datos_correlaciones <- datos_wide[,3:5]
coef_corr <- round(cor(datos_correlaciones), 2)
corrplot(coef_corr, 
         type = "upper",
         method = "number")

#### modelos con distintas estructuras de correlacion y  supuestos

# Simetria compuesta. Varianzas iguales (homocedasticidad) (es la utilizada en modelo condicional)
#Estima un parametro mas (rho), Asume igual correlación entre cualquier par de MR.
modelo_corCompSymm <- gls(t_reac ~ tratamiento * tiempo + inicial,
                          correlation = corCompSymm(form = ~ 1 | individuo),
                          data = datos_con_inicial)

# Estructura autoregresiva de primer orden. Varianzas iguales (homocedasticidad). 
#Estima un parametro mas (rho), plantea que la correlación entre tiempos disminuye exponencialmente
#según la distancia entre tiempos. La correlación entre las observaciones de dos tiempos 
# con la misma diferencia de tiempo es siempre la misma (rho=phi), sirve para MR igualmente espaciadas. ???
modelo_AR1 <- gls(t_reac ~ tratamiento * tiempo + inicial,
                  correlation = corAR1(form = ~ 1 | individuo),
                  data = datos_con_inicial)

# Modelo 3: Matriz de correlacion general. DESESTRUCTURADA.
#No hay restricciones sobre los parámetros de la matriz, pero es menos parsimoniosa, estima mas parametros.
modelo_corSymm <- gls(t_reac ~ tratamiento * tiempo + inicial,
                      correlation = corSymm(form = ~ 1 | individuo),
                      data = datos_con_inicial)

#SI NECECITO ADEMAS MODELAR VARIANZAS, BUSCO EL CODIGO EN SCRIPT DE TEORICA 11.

##### COMPROBACION DE SUPUESTOS. ----------------------------------------------------
.
#Para hacer esto en una forma mas simple: crear una funcion que realice los graficos y las 
#pruebas en un solo paso.

### Creacion de la funcion.

supuestos <- function (modelo) {
  
  residuos <- resid(modelo, type = "pearson")
  predichos <- predict(modelo)
  
  par(mfrow = c(1, 2))
  
  plot(x = predichos,
       y = residuos,
       # ylim = c(-4, 4),
       xlab = "Predichos",
       ylab = "Residuos de Pearson",
       main = "Grafico de dispersion de residuos v. predichos", 
       cex.main = 0.8 )
  
  abline(h = c(-2, 2, 0),
         col = c("red", "red", "black"),
         lty = c(2, 2, 1))
  
  #  qqnorm(residuos, cex.main = 0.8)
  #  qqline(residuos)
  qqPlot(residuos)
  shapiro.test(residuos)
  
}


supuestos(modelo_corCompSymm) #p-value = 0.1002 No evide para rehazar norm ni homocedasticidad.
supuestos(modelo_AR1) #p-value = 0.1217 No evide para rehazar norm ni homocedasticidad.
supuestos(modelo_corSymm) #p-value = 0.157 No evide para rehazar norm ni homocedasticidad.

#Todos cumplen los supuestos.

#Si no me pide rho, no hago los summary
summary(modelo_corCompSymm)
summary(modelo_AR1)
summary(modelo_corSymm)

#Solo hago el AIC para los modelos candidatos (es decir, los que cumplen los supuestos)
AIC(modelo_corCompSymm,modelo_AR1,modelo_corSymm)
BIC(modelo_corCompSymm,modelo_AR1,modelo_corSymm) #penaliza mas por los parametros (tener en cuenta que desestructurada estima muchos parametros)

# Seleccione el modelo mas apropiado

### Interpretacion del modelo seleccionado.
summary(modelo_AR1) #Veo el rho y el Residual standard error


getVarCov(modelo_AR1)  #Me devuelve la matriz de covarianza (donde veo las covarianzas)

# diagonal
4.3829*4.3829 #(residual st error al cuadrado=varianza) = 19.20981

# fuera de la diagonal
0.419038*19.20981     #(rho*varianza) = 8.04964
0.419038^2*19.20981   #(rho^2 * varianza) = 3.3731
#y asi continua...

anova(modelo_AR1)
# Denom. DF: 71 
# numDF  F-value p-value
# (Intercept)            1 808.1956  <.0001
# tratamiento            1  35.8014  <.0001
# tiempo                 2  32.9909  <.0001
# inicial                1   4.6201  0.0350
# tratamiento:tiempo     2   1.8450  0.1655

#Interaccion NS, hago comparaciones de efectos ppales. Si diese sig, hago efecto simples
#(cod script teo 11) o todos contra todos.

### Comparaciones.

# Paquete emmean
# comparaciones necesarias en funcion de los resultados obtenidos y la pregunta de interes

library(emmeans)

comp1_trat <- emmeans(modelo_AR1, pairwise ~ tratamiento)
comp1_trat
plot(comp1_trat$emmeans, comparisons = T)

#Hay diferencias entre grupo cafeina y placebo, la cafeina disminuye el tiempo de respuesta en 6.95 seg.

comp1_tiempo <- emmeans(modelo_AR1, pairwise ~   tiempo)
comp1_tiempo
plot(comp1_tiempo$emmeans, comparisons = T)

#Hay diferencias para todos los tiempos medidos, en el dia uno el promedio estimado de tiempo de 
#respuesta es de 13,4 seg, mientras que para dias posteriores va aumentando debido a la falta de 
#atencion por falta de suenio (tiempo respuesta prom dia 2 y 3= 19,4 y 22 seg respectivamente)

################ Predicciones
library("margins")
model_p <- prediction(modelo_AR1, at = list(inicial=rep(mean(datos_con_inicial$inicial),78)))
ggplot(data = model_p, aes(x = tiempo,
                           y = fitted,
                           colour = tratamiento,
                           group = tratamiento)) +
  labs(title = "Modelo marginal predicho con t_inicial fijo (valor medio: 6.58seg)",
       x = "",
       y = "Tiempo de reaccion (seg)") +
  geom_point() +
  geom_line() +
  theme_bw()


## Otro grafico alternativo

library(emmeans)
comp1 <- emmeans(modelo_AR1, pairwise ~ tratamiento*tiempo|inicial)
comp1
medidas_resumen <- as.data.frame(comp1$emmeans)
ggplot(medidas_resumen, aes(x=tiempo, y=emmean, fill=tratamiento)) +
  geom_point(aes(colour=tratamiento)) + geom_line(aes(colour=tratamiento)) +
  labs(x="tiempo") + labs(y="t de reaccion") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE, colour=tratamiento), width=0.2)+
  ggtitle("Comparación del t de reaccion en función del tiempo para ambos tratamientos", "Media ± SE") +
  theme_bw() + 
  annotate("text", x=c(1.2), y=c(26), label = c("Entre tiempos, p<0.05,  / Entre trat p<0.05")) +
  annotate("text", x=c(1,2,3),   y=c(20,25,27), label = c("A","B", "C")) +
  annotate("text", x=c(1,2,3),   y=c(12,18,22), label = c("a","b","c")) 

#otra opción:
library(ggeffects)
(a<-ggpredict(m5))
a <- ggpredict(m5, terms = c("tiempo", "droga"))
grafico_final<-plot(a, add.data = TRUE, grid = TRUE)
grafico_final + labs(x="Tiempo", y="Histamina (ug/ml)", title= "Valores predichos de histamina por el modelo")


#OOOOTRA OPCION
pred_modelo<-as.data.frame(compsimples$emmeans)
ggplot(pred_modelo, aes(x = Tiempo, y = emmean, fill = Exposicion)) +
  geom_point(aes(colour = Exposicion)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, colour = Exposicion), width = 0.2) +
  labs(x = "Tiempo") + labs(y = "Biomasa microbiana (µg C/g sustrato)") +
  ggtitle("Comparación de Biomasa microbiana en el tiempo para cada tratamiento")

#Hay diferencia entre dias de medicion, para todos los dias (letras distintas) pero tmb hay diferencias 
#para cada tratamiento (mayuscula vs minuscula)
