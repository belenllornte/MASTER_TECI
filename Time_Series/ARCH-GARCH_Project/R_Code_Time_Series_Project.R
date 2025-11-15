###################################################################################################
# LIBRERÍAS
###################################################################################################

install.packages("tidyverse") 
install.packages("astsa") 
install.packages("vars")
install.packages("mFilter") 
install.packages("dynlm") 
install.packages("rugarch") 
install.packages("rmgarch") 
install.packages("kableExtra") 
install.packages("FinTS") 
install.packages("fGarch")
install.packages("fGarch") 


library(tidyverse) 
library(lubridate) 
library(car) 
library(urca) 
library(tseries) 
library(astsa) 
library(forecast) 
library(foreign) 
library(timsac) 
library(vars) 
library(lmtest) 
library(mFilter) 
library(dynlm) 
library(nlme) 
library(quantmod) 
library(xts) 
library(broom) 
library(rugarch) 
library(rmgarch) 
library(kableExtra) 
library(knitr) 
library(MASS) 
library(FinTS) 
library(fGarch) 
library(parallel) 



###################################################################################################
# CARGAMOS Y VISUALIZAMOS LOS DATOS
###################################################################################################

# Cargar datos desde un archivo Excel
ue <- read_excel("C:/Users/belen/OneDrive/Escritorio/MÁSTER TECI/SERIES TEMPORALES/codigoarch/ue.xlsx")
# Vista previa de los datos
View(ue)

# Convertimos la base de datos en una serie temporal
ue <- ts(ue$`Unión Europea`, start=c(1960), end=c(2022), frequency=1)

# Graficamos la serie temporal
chartSeries(ue)

# ya son los rendimientos


###################################################################################################
# MODELADO ARIMA Y ANÁLISIS DE RESIDUOS
###################################################################################################

fitARIMA <- arima(ue, order=c(3,0,3), method="ML")
coeftest(fitARIMA)
# Estimate Std. Error z value  Pr(>|z|)    
# ar1       -0.079853   0.270056 -0.2957 0.7674681    
# ar2       -0.017446   0.310226 -0.0562 0.9551526    
# ar3        0.666990   0.198069  3.3675 0.0007586 ***
#   ma1        1.161325   0.312363  3.7179 0.0002009 ***
#   ma2        0.849260   0.429037  1.9795 0.0477647 *  
#   ma3        0.114976   0.241144  0.4768 0.6335093    
# intercept  4.662629   1.282033  3.6369 0.0002759 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



summary(fitARIMA)
# Call:
#   arima(x = ue, order = c(3, 0, 3), method = "ML")
# 
# Coefficients:
#   ar1      ar2     ar3     ma1     ma2     ma3  intercept
# -0.0799  -0.0174  0.6670  1.1613  0.8493  0.1150     4.6626
# s.e.   0.2701   0.3102  0.1981  0.3124  0.4290  0.2411     1.2820
# 
# sigma^2 estimated as 2.285:  log likelihood = -116.36,  aic = 248.73
# 
# Training set error measures:
#   ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
# Training set 0.0502101 1.511731 1.007061 -22.29465 63.50426 0.8980246 0.008637302

checkresiduals(fitARIMA)
# Ljung-Box test
# 
# data:  Residuals from ARIMA(3,0,3) with non-zero mean
# Q* = 1.3393, df = 4, p-value = 0.8547
# 
# Model df: 6.   Total lags used: 10



###################################################################################################
# ANÁLISIS DE HETEROCEDASTICIDAD (EFECTOS ARCH)
###################################################################################################


##Calculamos los residuos al cuadrado (saber si hay procesos residuales de volatilidad)
rescuad <- resid(fitARIMA)^2
rescuad
chartSeries(rescuad)


# Prueba de heterocedasticidad ARCH
fitARIMA.arch <- dynlm(rescuad ~ L(rescuad), data = ue)
summary(fitARIMA.arch)
# Time series regression with "ts" data:
#   Start = 1961, End = 2022
# 
# Call:
#   dynlm(formula = rescuad ~ L(rescuad), data = ue)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.4437 -1.9935 -1.7871 -0.4328 27.5707 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   2.0021     0.7827   2.558   0.0131 *
#   L(rescuad)    0.1549     0.1583   0.979   0.3317  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.707 on 60 degrees of freedom
# Multiple R-squared:  0.01571,	Adjusted R-squared:  -0.0006933 
# F-statistic: 0.9577 on 1 and 60 DF,  p-value: 0.3317 --> no hay efecto arch



## FAS y FAP: si hay barras significativas = no ruido blanco = heterocedasticidad
#Calculamos la FAS 
acfres2=acf(rescuad,lag.max = 200)
#Calculamos la FAP 
pacfres2=pacf(rescuad,lag.max = 200)


# Prueba ARCH de heterocedasticidad condicional
ueArchTest <- ArchTest(ue, lags=1, demean=TRUE)
ueArchTest


ueArchTest2 <- ArchTest(ue, lags=2, demean=TRUE)
ueArchTest2 


ueArchTest3 <- ArchTest(ue, lags=3, demean=TRUE)
ueArchTest3


ueArchTest20 <- ArchTest(ue, lags=20, demean=TRUE)
ueArchTest20
# ARCH LM-test; Null hypothesis: no ARCH effects
# 
# data:  ue
# Chi-squared = 36.467, df = 20, p-value = 0.01355

ueArchTest21 <- ArchTest(ue, lags=21, demean=TRUE)
ueArchTest21
# ARCH LM-test; Null hypothesis: no ARCH effects
# 
# data:  ue
# Chi-squared = 34.554, df = 21, p-value = 0.03158

ueArchTest22 <- ArchTest(ue, lags=22, demean=TRUE)
ueArchTest22
# ARCH LM-test; Null hypothesis: no ARCH effects
# 
# data:  ue
# Chi-squared = 26.646, df = 22, p-value = 0.225

ueArchTest50 <- ArchTest(ue, lags=25, demean=TRUE)
ueArchTest50
# ARCH LM-test; Null hypothesis: no ARCH effects
# 
# data:  ue
# Chi-squared = 22.485, df = 25, p-value = 0.6076



########################################### TENGO EFECTO ARCH DE ORDEN 21 ---> hacemos un GARCH mejor

# En este caso, es mejor modelar un GARCH en lugar de un ARCH(21).
# Un ARCH(21) requiere estimar 21 coeficientes alpha_i, mientras que un GARCH(1,1) solo necesita 
# 2 parámetros (beta1 y alpha1). Esto hace que GARCH sea más parsimonioso y reduzca el riesgo 
# de sobreajuste.

# GARCH es la mejor opción porque es más eficiente, captura mejor la persistencia y evita 
# estimar un número excesivo de parámetros.

library(rugarch)

# GARCH(1,1) + ARMA(3,3)
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(3,3)))

fit_garch <- ugarchfit(spec_garch, data = ue)
fit_garch

ug_res = (fit_garch@fit$residuals)^2
ug_res
# [1] 3.098284e+00 2.028451e+00 2.716858e-03 5.408210e-02 3.777737e-01 6.018166e-04
# [7] 7.583333e-01 1.106835e-02 1.208942e+00 1.081582e+00 1.017674e-03 4.359996e+00
# [13] 3.293664e+00 4.562664e-03 1.999641e+01 2.486147e-01 6.733890e+00 1.221594e+00
# [19] 6.318535e+00 2.150311e-02 1.668380e+00 1.185752e+00 9.640287e-02 4.273656e-01
# [25] 1.170706e+00 1.063690e+00 1.394421e-01 2.531685e-01 4.877918e-02 9.136796e-01
# [31] 8.775192e-01 2.484360e-01 4.620781e-02 6.008980e-02 5.258224e-02 5.206010e-02
# [37] 1.474309e+00 6.696038e-02 1.905547e-01 3.311165e-01 1.705580e-01 4.159412e-01
# [43] 2.662915e-03 1.534858e+00 1.518696e-01 1.875250e+00 1.473438e-01 5.001345e+00
# [49] 1.247812e+00 1.703331e-02 4.206397e-01 1.040779e-02 2.047843e-03 9.170969e-01
# [55] 2.496917e+00 1.126976e-01 7.320299e-01 1.893407e-01 2.253152e+00 2.264546e-01
# [61] 8.260986e-01 4.221967e+00 1.627324e+01

ug_var = fit_garch@fit$var
ug_var
# [1]  1.5942157  1.5942157  1.5942157  1.0782249  0.8479358  0.8963247  0.7307723
# [8]  1.0294443  0.8021052  1.2913291  1.4701373  1.0157861  2.9811315  3.4206591
# [15]  1.9856849 11.3215165  6.0298393  6.6629321  4.2066360  5.5492474  3.0506975
# [22]  2.6383069  2.1910491  1.4215430  1.2059527  1.4725560  1.5510941  1.1255411
# [29]  0.9714831  0.7922906  1.1380564  1.2914985  1.0514753  0.8307014  0.7281001
# [36]  0.6734020  0.6459908  1.3472189  0.9879204  0.8717065  0.8846727  0.8104103
# [43]  0.8968827  0.7320852  1.4203835  1.0669108  1.7576537  1.2320359  3.4108115
# [50]  2.6056644  1.5874403  1.2849133  0.9285720  0.7475047  1.1175451  2.0952402
# [57]  1.3821793  1.3395417  1.0456193  1.9370226  1.3608252  1.3762225  3.0906550

plot(ug_res, type = "l", main = "Residuos al cuadrado vs. Varianza estimada")
lines(ug_var, col = "green")

## Pronósticos del modelo 
ugfore=ugarchforecast(fit_garch,n.head=5)
ugfore
plot(ugfore)

