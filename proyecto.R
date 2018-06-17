datos <- read.table( 'Capacitacion.txt', header = T )
attach(datos)
#PARTE 1
#Analisis descriptivo
summary(datos)
#Desviaciones estandard
sd(datos$JTS)
sd(datos$ING)
sd(datos$EDU)
sd(datos$DIS)
sd(datos$SEM)
sd(datos$EMP)
sd(datos$PAS)
sd(datos$IPR)
# Histogramas
par(mfrow = c(2,4))
hist(datos$JTS, ylab='Frecuencias', xlab = 'JTS', main = '')
hist(datos$ING, ylab='Frecuencias', xlab = 'ING', main = '')
hist(datos$EDU, ylab='Frecuencias', xlab = 'EDU', main = '')
hist(datos$DIS, ylab='Frecuencias', xlab = 'DIS', main = '')
hist(datos$SEM, ylab='Frecuencias', xlab = 'SEM', main = '')
hist(datos$EMP, ylab='Frecuencias', xlab = 'EMP', main = '')
hist(datos$PAS, ylab='Frecuencias', xlab = 'PAS', main = '')
hist(datos$IPR, ylab='Frecuencias', xlab = 'IPR', main = '')
mtext("Histogramas de todas las variables", side = 3, line = -2, outer = TRUE)
# Boxplots
par(mfrow = c(2,4))
boxplot(datos$JTS, xlab = 'JTS')
boxplot(datos$ING, xlab = 'ING')
boxplot(datos$EDU, xlab = 'EDU')
boxplot(datos$DIS, xlab = 'DIS')
boxplot(datos$SEM, xlab = 'SEM')
boxplot(datos$EMP, xlab = 'EMP')
boxplot(datos$PAS, xlab = 'PAS')
boxplot(datos$IPR, xlab = 'IPR')
mtext("Boxplots de todas las variables", side = 3, line = -2, outer = TRUE)
boxplot(datos)
par(mfrow = c(1,1))
# Grafico de barras para la zona
barplot(table(datos$zona), ylim = c(0, 100), xlab = 'Zonas', main = 'Frecuencias de la variable zona')
#PARTE 2 graph de dispersion y matriz de correlacion
pairs(datos)
cor(datos[,1:8]) #sin la columna zona ya que es cualitativa
# a primera vista se observa que las variables que mejor se ajustan con JTS
# son ING, EDU, DIS y PAS
#PARTE 3 Prueba de bondad de ajuste
foo = datos$JTS
hist(foo, ylim=c(0,50), main = 'Puntaje en la prueba de capacitación.', ylab = 'Frecuencias')
hist(foo, plot = F)
fi = c(31,33,34,34,22,40,32,45,29)
qqnorm(fi)
qqline(fi)
# los datos estan muy alejados de la linea, no sigue una dist normal
# se realiza la prueba chi cuadrado
(k = length(fi))
n = sum(fi)
mi = c(150,250,350,450,550,650,750,850,950)
(xbarra = sum(fi * mi) / n)
x_barra = rep(xbarra, k)
S_cuadrado = sum(fi * (mi - x_barra) ^ 2) / (n - 1)
(S = sqrt(S_cuadrado))
(pi = pnorm(2 : 10 * 100, xbarra, S) - pnorm(1 : 9 * 100, xbarra, S))
chi2_obs = sum((fi - n * pi) ^ 2 / ( n * pi))
chi2_obs
r = 2
chi2_alpha = qchisq(1 - 0.05, k- 1 - r)
chi2_alpha
(p_valor = 1 - pchisq(chi2_obs, k - 1 - r))
#como pvalor=1.030287e-13 < 0.05, se rechaza que tenga dist normal
#PARTE 4
#como zona es una variable o factor cualitativo, no se incluye en el estudio
modelo0=lm(JTS ~ ING + EDU + DIS + SEM + EMP + PAS + IPR)
summary(modelo0)
#R2 91.17%
# se procede a quitar la variable SEM ya que no es significativa
modelo1=lm(JTS ~ ING + EDU + DIS + EMP + PAS + IPR)
summary(modelo1)
#R2 91.18%
# se procede a quitar la variable IPR
modelo2=lm(JTS ~ ING + EDU + DIS + PAS + EMP)
summary(modelo2)
#R2 91.17%
# se procede a quitar la variable EMP
modelo3=lm(JTS ~ ING + EDU + DIS + PAS)
summary(modelo3)
par(mfrow=c(2, 2))
plot(modelo3)
par(mfrow=c(1, 1))
#R2 91.11%, todas las variables son significativas.
#chequeo de normalidad de errores
qqnorm(rstandard(modelo3))
qqline(rstandard(modelo3))
boxplot(datos$ING + datos$EDU + datos$DIS + datos$PAS, ylab = 'Valores del modelo', main = 'Boxplot del modelo escogido')
#homocedasticidad
plot(fitted.values(modelo3), rstandard(modelo3), main = 'Homocedasticidad', ylab = 'Errores estándar', xlab = 'x')
# independecia de errores
par(mfrow = c(2,2))
#mtext("My 'Title' in a strange place", side = 3, line = -2, outer = TRUE)
plot(datos$ING, rstandard(modelo3), sub = 'Independencia de errores ING', xlab = 'ING', ylab = 'Errores estándar')
plot(datos$EDU, rstandard(modelo3), sub = 'Independencia de errores EDU', xlab = 'EDU', ylab = 'Errores estándar')
plot(datos$DIS, rstandard(modelo3), sub = 'Independencia de errores DIS', xlab = 'DIS', ylab = 'Errores estándar')
plot(datos$PAS, rstandard(modelo3), sub = 'Independencia de errores PAS', xlab = 'PAS', ylab = 'Errores estándar')
mtext("Independencia de Errores", side = 3, line = -2, outer = TRUE)
par(mfrow = c(1,1))
# se observa los valores no siguen patron alguno ni en homocedasticidad ni en independencia, son discretos
# PARTE 5
pred <- read.table( "Capacitacion_prediccion.txt", header = T)
attach(pred)
new <- data.frame(pred$JTS,  pred$ING, pred$EDU, pred$DIS, pred$PAS)
JTS1 <- predict(modelo3,new, interval = "confidence")
JTS2 <- predict(modelo3,new, interval = "prediction")
matplot(new$pred.JTS,cbind(JTS1, JTS2[,-1]), lty=c(1,2,2,3,3), type="l", ylab="predicted JTS", main = 'Bandas de confianza de nuestro modelo')
Y_ING = pred$ING
Y_EDU = pred$EDU
Y_DIS = pred$DIS
Y_PAS = pred$PAS
#Valores de la prediccion del modelo
y = -287.738 + 10.800*Y_ING + 8.397*Y_EDU - 11.090*Y_DIS + 7.945*Y_PAS 
#Valores observados
Y_Y = pred$JTS
boxplot(Y_Y-y, main = 'Boxplot de las diferencias real - predicción')
hist(Y_Y-y, main = 'Diferencias entre real - predicción', ylab = 'Diferencias', xlab = 'Frecuencias')
#PARTE 6
summary(datos$IPR)
empleoA = "EmpleoA"
empleoB = "EmpleoB"
empleoC = "EmpleoC"
empleoD = "EmpleoD"
datos["parte6"] = cut(datos$IPR, breaks = c(97,840,1200,1550,2530), labels = c(empleoD, empleoC, empleoB, empleoA))
summary(datos$parte6)
barplot(table(datos$parte6), ylim = c(0, 100), main = 'Tipo de Empleo segun salario')
#PARTE 7
medAA = median(datos$JTS[(datos$zona == "A") & datos$parte6 == empleoA])
medAB = median(datos$JTS[(datos$zona == "A") & datos$parte6 == empleoB])
medAC = median(datos$JTS[(datos$zona == "A") & datos$parte6 == empleoC])
medAD = median(datos$JTS[(datos$zona == "A") & datos$parte6 == empleoD])
medBA = median(datos$JTS[(datos$zona == "B") & datos$parte6 == empleoA])
medBB = median(datos$JTS[(datos$zona == "B") & datos$parte6 == empleoB])
medBC = median(datos$JTS[(datos$zona == "B") & datos$parte6 == empleoC])
medBD = median(datos$JTS[(datos$zona == "B") & datos$parte6 == empleoD])
medCA = median(datos$JTS[(datos$zona == "C") & datos$parte6 == empleoA])
medCB = median(datos$JTS[(datos$zona == "C") & datos$parte6 == empleoB])
medCC = median(datos$JTS[(datos$zona == "C") & datos$parte6 == empleoC])
medCD = median(datos$JTS[(datos$zona == "C") & datos$parte6 == empleoD])
medDA = median(datos$JTS[(datos$zona == "D") & datos$parte6 == empleoA])
medDB = median(datos$JTS[(datos$zona == "D") & datos$parte6 == empleoB])
medDC = median(datos$JTS[(datos$zona == "D") & datos$parte6 == empleoC])
medDD = median(datos$JTS[(datos$zona == "D") & datos$parte6 == empleoD])
# PARTE 8
# Realice un análisis de varianza para estudiar si existen diferencias entre los puntajes medio de los
# aspirantes según la clasificación del ingreso y zona.
#empleos
medias = c(medAA, medAB, medAC, medAD,
 medBA, medBB, medBC, medBD,
 medCA, medCB, medCC, medCD,
 medDA, medDB, medDC, medDD)
empleos <- gl(4, 4)
zona <- factor(rep(1:4, 4))
anova_formula = medias ~ zona + empleos
xtabs(anova_formula)
mod.lm = lm(anova_formula)
anova(mod.lm)
# Asumiendo un nivel de significancia del 5%, no se puede rechazar las hipotesis de que son iguales
# las medias de las poblaciones, tanto por zona como por clasificacion del ingreso.
# PARTE 9
# En el caso de conseguir diferencias significativas, realice pruebas de medias para determinar cuales
# son las medias que difieren.
# No se consiguieron diferencias significativas, por lo cual no hace falta realizar
# pruebas de medias.