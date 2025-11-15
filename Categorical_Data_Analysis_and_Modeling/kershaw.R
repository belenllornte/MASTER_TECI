################################################################################
#                             LIBRERÍAS
################################################################################

# Instalación de librerías
install.packages("Stat2Data")
install.packages("ROSE")
install.packages("nnet")  # Para multinom()  
install.packages("dplyr") # Para manipulación de datos  
install.packages("ROCR")

# Cargar librerías  
library(ROCR)
library(nnet)  
library(ROSE)
library(Stat2Data) # para la base de datos
library(vcd)
library(caret)
library(dplyr)
library(MASS)


################################################################################
#                           CARGAR LOS DATOS
################################################################################

data(Kershaw)

head(Kershaw)  # Muestra las primeras filas
str(Kershaw)   # Estructura del dataset
summary(Kershaw) # Estadísticas resumidas

colnames(Kershaw) 
View(Kershaw)
################################################################################
#                    ANÁLISIS EXPLORATORIO DE LOS DATOS
################################################################################

# Vemos las categorías de las variables de tipo factor

levels(Kershaw$Outcome)

levels(Kershaw$Class)

levels(Kershaw$PitchType)

levels(Kershaw$Count)

levels(Kershaw$ABEvent)

levels(Kershaw$Time)

Kershaw$Time <- as.POSIXct(Kershaw$Time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

str(Kershaw) # y vemos que "Time" ya no es de tipo factor.

colSums(is.na(Kershaw)) # no hay valores faltantes

# boxplots para detectar valores atípicos:

# Abre una nueva ventana de gráficos más grande
dev.new(width = 12, height = 10)

# Seleccionar solo las variables numéricas
numeric_vars <- Kershaw[sapply(Kershaw, is.numeric)]  # Filtra las columnas numéricas

# Ajustar el layout a 4 filas y 4 columnas
par(mfrow = c(4, 4))  # 4 filas, 4 columnas

# Generar los boxplots
for (col in names(numeric_vars)) {
  boxplot(numeric_vars[[col]], main = col, col = "lightblue")
}

################################################################################
#                  GRÁFICAS DE LAS VARIABLES CATEGÓRICAS
################################################################################

# Gráfico de barras para la variable 'Result'
ggplot(Kershaw, aes(x = Result)) + 
  geom_bar(fill = "red") + 
  labs(title = "Distribución de Result", x = "Result", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'BatterHand'
ggplot(Kershaw, aes(x = BatterHand)) + 
  geom_bar(fill = "lightsalmon") + 
  labs(title = "Distribución de BatterHand", x = "BatterHand", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'Outcome'
ggplot(Kershaw, aes(x = Outcome)) + 
  geom_bar(fill = "lightblue") + 
  labs(title = "Distribución de Outcome", x = "Outcome", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'Class'
ggplot(Kershaw, aes(x = Class)) + 
  geom_bar(fill = "lightgreen") + 
  labs(title = "Distribución de Class", x = "Class", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'Swing'
ggplot(Kershaw, aes(x = Swing)) + 
  geom_bar(fill = "lightcoral") + 
  labs(title = "Distribución de Swing", x = "Swing", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

# Gráfico de barras para la variable 'PitchType'
ggplot(Kershaw, aes(x = PitchType)) + 
  geom_bar(fill = "gold") + 
  labs(title = "Distribución de PitchType", x = "PitchType", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

Kershaw$PitchType_Regrouped <- ifelse(Kershaw$PitchType == "FF", "Fastball", "Offspeed/Breaking")

# Verificar distribución
table(mini_kershaw$PitchType_Regrouped)

# Gráfico de barras para la variable 'PitchType'
ggplot(Kershaw, aes(x = PitchType_Regrouped)) + 
  geom_bar(fill = "yellow") + 
  labs(title = "Distribución de PitchType_Regrouped", x = "PitchType_Regrouped", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'Count'
ggplot(Kershaw, aes(x = Count)) + 
  geom_bar(fill = "lightpink") + 
  labs(title = "Distribución de Count", x = "Count", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

# Gráfico de barras para la variable 'InningSide'
ggplot(Kershaw, aes(x = InningSide)) + 
  geom_bar(fill = "lightseagreen") + 
  labs(title = "Distribución de InningSide", x = "InningSide", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Gráfico de barras para la variable 'ABEvent'
ggplot(Kershaw, aes(x = ABEvent)) + 
  geom_bar(fill = "violet")+
  labs(title = "Distribución de ABEvent", x = "ABEvent", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

# Gráfico de barras para la variable 'Batter'
ggplot(Kershaw, aes(x = Batter)) + 
  geom_bar(fill = "darkblue") + 
  labs(title = "Distribución de Batter", x = "Batter", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas





# grafico la velocidad 

ggplot(Kershaw, aes(x = "", y = StartSpeed)) +
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.6) +
  labs(title = "Distribución de StartSpeed",
       y = "StartSpeed", x = "") +
  theme_minimal()



# ------------------------------------------------------ corte binario con la mediana
corte <- median(Kershaw$StartSpeed, na.rm = TRUE) # 91.7

Kershaw$StartSpeed_binaria <- ifelse(Kershaw$StartSpeed >= corte, "Alta", "Media")

table(Kershaw$StartSpeed_binaria)

# Gráfico de barras para la variable 'StartSpeed_binaria'
ggplot(Kershaw, aes(x = StartSpeed_binaria)) + 
  geom_bar(fill = "lightpink") + 
  labs(title = "Distribución de StartSpeed_binaria", x = "StartSpeed_binaria", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

ggplot(Kershaw, aes(x = "", y = StartSpeed)) +
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.6) +  # Puntos dispersos
  geom_hline(yintercept = corte, linetype = "dashed", color = "red") +  # Línea roja en el corte
  labs(title = "Distribución de StartSpeed con corte binario (mediana)",
       y = "StartSpeed", x = "") +
  theme_minimal()

# -----------------------------------------------------------------------------

Kershaw$StartSpeed_cat <- cut(Kershaw$StartSpeed,
                              breaks = quantile(Kershaw$StartSpeed, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                              labels = c("Baja", "Media", "Alta"),
                              include.lowest = TRUE)

table(Kershaw$StartSpeed_cat)

# Gráfico de barras para la variable 'StartSpeed_binaria'
ggplot(Kershaw, aes(x = StartSpeed_cat)) + 
  geom_bar(fill = "lightpink") + 
  labs(title = "Distribución de StartSpeed_cat", x = "StartSpeed_cat", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas

# Calculamos los puntos de corte
cortes <- quantile(Kershaw$StartSpeed, probs = c(1/3, 2/3), na.rm = TRUE)

# Gráfico
ggplot(Kershaw, aes(x = "", y = StartSpeed)) +
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.6) +
  geom_hline(yintercept = cortes, linetype = "dashed", color = "red") +
  labs(title = "Distribución de StartSpeed con líneas de terciles",
       y = "StartSpeed", x = "") +
  theme_minimal()

#--------------------------------------------------------------------------- -inf-80-90-+inf

ggplot(Kershaw, aes(x = "", y = StartSpeed)) +
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.6) +
  geom_hline(yintercept = c(80, 90), linetype = "dashed", color = "red") +
  labs(title = "Distribución de StartSpeed con cortes en 80 y 90 mph",
       y = "StartSpeed", x = "") +
  theme_minimal()


# Definir categorías basadas en los valores 80 y 90
Kershaw$StartSpeed_categorica <- cut(Kershaw$StartSpeed,
                              breaks = c(-Inf, 80, 90, Inf),
                              labels = c("Bajo", "Medio", "Alto"),
                              include.lowest = TRUE)

# Tabla de frecuencias para las categorías
table(Kershaw$StartSpeed_categorica)

# Gráfico de barras para las categorías
ggplot(Kershaw, aes(x = StartSpeed_categorica)) + 
  geom_bar(fill = "lightpink") + 
  labs(title = "Distribución de StartSpeed categorizado por cortes 80 y 90",
       x = "Categoría de StartSpeed", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


################################################################################
#                                PRÁCTICA 1
#                          TABLAS DE CONTINGENCIA
################################################################################

# 1. Plantea el objetivo del estudio. Construye las correspondientes tablas de 
# contingencia. Compara grupos empleando las diferentes medidas que hemos estudiado 
# (DP, RR, OR), junto con los correspondientes IC 95%. 
# 
# 2. Valora la influencia de una tercera variable, como posible variable de 
# confusión o interacción. 
# 
# 3. Analiza la hipótesis de independencia entre las variables aplicando el test que 
# corresponda según los datos de la tabla (Test de chi-cuadrado, Test exacto de 
# Fisher, el estadístico de Mantel-Haenszel). 
# 
# 4. Interpreta los resultados.



# Lista de variables categóricas
categorical_vars <- c("Outcome", "Class", "Swing", "PitchType", "Count", 
                      "InningSide", "BatterHand", "ABEvent", "Batter")

# Crear tablas de contingencia para cada variable categórica con la variable "Result"
contingency_tables <- lapply(categorical_vars, function(var) {
  table(Kershaw[[var]], Kershaw$Result)
})

# Mostrar las tablas de contingencia
names(contingency_tables) <- categorical_vars
contingency_tables



# Las tablas de contingencia de tamaño 2x2 son con las vars: Swing(No, Yes), InningSide (bottom, top), 
# BatterHand (L(left), R (right)) y estas se les puede aplicar las funciones de DP, RR, OR.
# La variable objetivo dicotómica es Result (Neg, Pos)
 

####### INFERENCIA EN TABLAS DE CONTINGENCIA: CALCULO DE LOS IC DEL OR, DR Y RR

####################################################### Función de Laura A. Thompson

Wald.ci = function (Table , aff.response , alpha =.05 ){
  # Gives two - sided Wald CI 's for odds ratio ,
  # difference in proportions and relative risk.
  # Table is a 2x2 table of counts with rows giving
  # the treatment populations
  # aff.response is a string like "c (1 ,1)" giving the cell
  # of the beneficial response and the treatment category
  # alpha is significance level
  pow = function (x, a= -1) x^a
  z.alpha = qnorm (1- alpha /2)
  if( is.character ( aff.response ))
    where = eval ( parse ( text = aff.response ))
  else where = aff.response
  Next = as.numeric ( where ==1) + 1
  
  # OR
  odds.ratio = Table [ where [1] , where [2]] * Table [ Next [1] , Next [2]] /
    ( Table [ where [1] , Next [2]] * Table [ Next [1] , where [2]])
  se.OR = sqrt (sum(pow( Table )))
  ci.OR = exp(log( odds.ratio ) + c( -1 ,1)* z.alpha * se.OR )
  
  # difference of proportions
  p1 = Table [ where [1] , where [2]] /(n1= Table [ where [1] , Next [2]] +
                                          Table [ where [1] , where [2]])
  p2= Table [ Next [1] , where [2]] /(n2= Table [ Next [1] , where [2]] +
                                        Table [ Next [1] , Next [2]])
  se.diff = sqrt (p1*(1- p1)/n1 + p2*(1- p2)/n2)
  ci.diff = (p1 -p2) + c( -1 ,1)* z.alpha * se.diff
  
  # relative risk
  RR = p1/p2
  se.RR = sqrt ((1 - p1)/(p1*n1) + (1- p2)/(p2*n2 ))
  ci.RR = exp(log(RR) + c( -1 ,1)* z.alpha * se.RR )
  list (OR= list ( odds.ratio = odds.ratio , CI= ci.OR ),
        proportion.difference = list ( diff =p1 -p2 , CI= ci.diff ),
        relative.risk = list ( relative.risk =RR , CI= ci.RR ))
}

#################################################################################





#################### Tabla de contingencia: Result vs Swing #####################
tabla_swing_result <- table(Kershaw$Swing, Kershaw$Result)
print(tabla_swing_result)


# Convertir la tabla de contingencia en un formato adecuado para ggplot
df_swing_result <- as.data.frame(tabla_swing_result)
colnames(df_swing_result) <- c("Swing", "Result", "Freq")

# Graficar
ggplot(df_swing_result, aes(x = Result, y = Freq, fill = Swing)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" coloca las barras una al lado de la otra
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Diferentes colores para cada categoría de "Swing"
  labs(title = "Resultado vs Swing", x = "Resultado", y = "Frecuencia") +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5) +  # Añadir los números
  theme_minimal() +
  theme(legend.title = element_blank())  # Eliminar el título de la leyenda


Wald.ci(tabla_swing_result, c(1, 1))

res=chisq.test(tabla_swing_result)

res

res$expected

# Estratificar por batterhand
# ---------------------------------------------------------------

# Crear las tablas de contingencia para los estratos L y R
tabla_batterhand_result_f <- table(Kershaw$Swing[Kershaw$BatterHand == "L"], Kershaw$Result[Kershaw$BatterHand == "L"])
tabla_batterhand_result_o <- table(Kershaw$Swing[Kershaw$BatterHand == "R"], Kershaw$Result[Kershaw$BatterHand == "R"])

# Combinar las tablas en una tabla 3D
tabla_3d <- array(c(tabla_batterhand_result_f, tabla_batterhand_result_o), 
                  dim = c(2, 2, 2), 
                  dimnames = list(Swing = c("No", "Yes"), 
                                  Result = c("Neg", "Pos"), 
                                  BatterHand = c("L", "R")))

# Aplicar el test de Cochran-Mantel-Haenszel
mantelhaen.test(tabla_3d)

# estratificar por startspeed
# ---------------------------------------------------------------

# Crear tablas de contingencia por cada nivel de StartSpeed_categorica
tabla_bajo <- table(Kershaw$Swing[Kershaw$StartSpeed_categorica == "Bajo"],
                    Kershaw$Result[Kershaw$StartSpeed_categorica == "Bajo"])

tabla_medio <- table(Kershaw$Swing[Kershaw$StartSpeed_categorica == "Medio"],
                     Kershaw$Result[Kershaw$StartSpeed_categorica == "Medio"])

tabla_alto <- table(Kershaw$Swing[Kershaw$StartSpeed_categorica == "Alto"],
                    Kershaw$Result[Kershaw$StartSpeed_categorica == "Alto"])

# Combinar en un array 3D
tabla_3d_swing_speed <- array(c(tabla_baja, tabla_media, tabla_alta),
                              dim = c(2, 2, 3),
                              dimnames = list(
                                Swing = c("No", "Yes"),
                                Result = c("Neg", "Pos"),
                                StartSpeed = c("Baja", "Media", "Alta")
                              ))

tabla_bajo
tabla_medio
tabla_alto


# Test de Cochran-Mantel-Haenszel
mantelhaen.test(tabla_3d_swing_speed)



################## Tabla de contingencia: Result vs InningSide ###################

# ¿El lanzador tiene más éxito (Pos) cuando lanza en casa (bottom) que cuando lanza como visitante (top)?
# Tabla de contingencia: Result vs InningSide
tabla_inningside_result <- table(Kershaw$InningSide, Kershaw$Result)
print(tabla_inningside_result)

# Convertir la tabla de contingencia en un formato adecuado para ggplot
df_inningside_result <- as.data.frame(tabla_inningside_result)
colnames(df_inningside_result) <- c("InningSide", "Result", "Freq")

# Graficar con números sobre las barras
ggplot(df_inningside_result, aes(x = Result, y = Freq, fill = InningSide)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" coloca las barras una al lado de la otra
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Diferentes colores para cada categoría de "InningSide"
  labs(title = "Resultado vs InningSide", x = "Resultado", y = "Frecuencia") +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5) +  # Añadir los números
  theme_minimal() +
  theme(legend.title = element_blank())  # Eliminar el título de la leyenda

Wald.ci(tabla_inningside_result, c(1, 1))

res=chisq.test(tabla_inningside_result)

res

res$expected

# no hacemos estratificacion pq no tiene sentido

################## Tabla de contingencia: Result vs BatterHand ###################

# ¿La mano del bateador influye?
# Tabla de contingencia: Result vs BatterHand
tabla_batterhand_result <- table(Kershaw$BatterHand, Kershaw$Result)
print(tabla_batterhand_result)
# Se compararon los jugadores que batean con la mano izquierda (L) 
# y los que batean con la mano derecha (R) con respecto al resultado 
# del juego (Result: Negativo o Positivo).

# Convertir la tabla de contingencia en un formato adecuado para ggplot
df_batterhand_result <- as.data.frame(tabla_batterhand_result)
colnames(df_batterhand_result) <- c("BatterHand", "Result", "Freq")

# Graficar con números sobre las barras
ggplot(df_batterhand_result, aes(x = Result, y = Freq, fill = BatterHand)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" coloca las barras una al lado de la otra
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Diferentes colores para cada categoría de "BatterHand"
  labs(title = "Resultado vs BatterHand", x = "Resultado", y = "Frecuencia") +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5) +  # Añadir los números
  theme_minimal() +
  theme(legend.title = element_blank())  # Eliminar el título de la leyenda



#Wald.ci(tabla_batterhand_result, c(1,1))  # Análisis para la primera categoría -- Left
Wald.ci(tabla_batterhand_result, c(2,1))  # Análisis para la segunda categoría -- Right

# Prueba de independencia Chi-cuadrado
(res = chisq.test(tabla_batterhand_result))
res
# Recuentos esperados
res$expected


# El lugar de la entrada puede tener efectos psicológicos o de rendimiento en los 
# jugadores. Los lanzadores pueden sentirse más cómodos en su propio estadio ("bottom") 
# y, por lo tanto, tener un mejor desempeño. Esto puede influir en la efectividad del 
# lanzamiento y, en consecuencia, en los resultados.

# Estratificar por InningSide (bottom, top)
# ---------------------------------------------------------------
tabla_batterhand_result_top <- table(mini_kershaw$BatterHand[mini_kershaw$InningSide == "top"], mini_kershaw$Result[mini_kershaw$InningSide == "top"])
tabla_batterhand_result_top
tabla_batterhand_result_bottom <- table(mini_kershaw$BatterHand[mini_kershaw$InningSide == "bottom"], mini_kershaw$Result[mini_kershaw$InningSide == "bottom"])
tabla_batterhand_result_bottom

# Calcular OR, RR y DP para cada estrato
Wald.ci(tabla_batterhand_result_top, c(1, 1))  # Estrato top
Wald.ci(tabla_batterhand_result_bottom, c(1, 1))  # Estrato bottom

# Si los OR o RR cambian significativamente entre los estratos, 
# esto indica que InningSide es una variable de interacción que 
# afecta la relación entre BatterHand y Result.



# Estratificar por PitchType
# ---------------------------------------------------------------
tabla_batterhand_result_f <- table(Kershaw$BatterHand[Kershaw$PitchType_Regrouped == "Fastball"], Kershaw$Result[Kershaw$PitchType_Regrouped == "Fastball"])
tabla_batterhand_result_f
tabla_batterhand_result_o <- table(Kershaw$BatterHand[Kershaw$PitchType_Regrouped == "Offspeed/Breaking"], Kershaw$Result[Kershaw$PitchType_Regrouped == "Offspeed/Breaking"])
tabla_batterhand_result_o



# Estratificar por tipo de lanzamiento
# ---------------------------------------------------------------

# Obtener niveles únicos del tipo de lanzamiento
niveles_pitch <- unique(Kershaw$PitchType_Regrouped)

# Crear lista de tablas 2x2 por tipo de pitch
tablas_list <- list()

for (i in seq_along(niveles_pitch)) {
  pitch_actual <- niveles_pitch[i]
  datos_filtrados <- Kershaw[Kershaw$PitchType_Regrouped == pitch_actual, ]
  
  tabla <- table(datos_filtrados$BatterHand, datos_filtrados$Result)
  
  if (all(c("L", "R") %in% rownames(tabla)) && all(c("Neg", "Pos") %in% colnames(tabla))) {
    tablas_list[[i]] <- tabla
  }
}

# Convertir a array 3D
n_validas <- length(tablas_list)
tabla_3d <- array(unlist(tablas_list),
                  dim = c(2, 2, n_validas),
                  dimnames = list(BatterHand = c("L", "R"),
                                  Result = c("Neg", "Pos"),
                                  PitchType = niveles_pitch[1:n_validas]))

# Aplicar test CMH
resultado_cmh <- mantelhaen.test(tabla_3d)
print(resultado_cmh)






# Proporciones conjuntas
# prop.table(tabla_kershaw)
# Proporciones marginales para 'BatterHand'
# marginal_sums <- marginSums(tabla_kershaw, margin = 1)  # Sumas marginales de las filas
# proporciones_marginales <- marginal_sums / sum(tabla_kershaw)  # Proporción respecto al total
# Cociente de verosimilitudes
# assocstats(tabla_kershaw)




################################################################################
#                              PRÁCTICA 2
#                          REGRESIÓN LOGÍSTICA
################################################################################


# 1) Establece una pregunta de investigación (variable respuesta: binaria) a la 
# que puedas responder con los datos que elegiste y sobre los que trabajaste en 
# la Tarea 1 (también
#                                                                                                                                                                         puedes trabajar sobre otra base de datos si esta anterior no te convence).
# 2) Describe brevemente las variables que figuran en la base de datos elegida.
#
# 3) En función de tu pregunta de investigación, explica si aplicarás un modelo 
# predictivo o un modelo explicativo.
#
# 4) Aplica un modelo de Regresión Logística.
# Para ello, deberás describir el procedimiento seguido en la elección de las 
# variables incluidas en el modelo máximo inicial (cribado previo: tablas de 
# contingencia, modelos univariantes... o variable con justificación 
# teórica/práctica). Si descartas alguna variable desde el principio, también 
# deberás justificarlo.
#
# 5) Describe los pasos seguidos hasta alcanzar el modelo final e interpreta los 
# resultados del mismo (en base a tu pregunta de investigación).
# En caso de que sea un modelo predictivo, describe su ajuste y potencia predictiva, 
# y lleva a cabo una predicción que te resulte de interés.

# Modelo nulo: solo el intercepto
modelo_nulo <- glm(Result ~ 1, data = Kershaw, family = binomial)



### MODELOS UNIVARIANTES

rgswing=glm(Result ~ Swing, data = Kershaw, family = binomial(link=logit))
summary(rgswing)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgswing, test = "Chisq")

rgClass=glm(Result ~ Class, data = Kershaw, family = binomial(link=logit))
summary(rgClass)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgClass, test = "Chisq")

rgbatterhand=glm(Result ~ BatterHand, data = Kershaw, family = binomial(link=logit))
summary(rgbatterhand)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgbatterhand, test = "Chisq")

rgoutcome=glm(Result ~ Outcome, data = Kershaw, family = binomial(link=logit))
summary(rgoutcome) # sobreajuste a mis datos

rgPitchType=glm(Result ~ PitchType, data = Kershaw, family = binomial(link=logit))
summary(rgPitchType)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgPitchType, test = "Chisq")

rgInningSide=glm(Result ~ InningSide, data = Kershaw, family = binomial(link=logit))
summary(rgInningSide)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgInningSide, test = "Chisq")

rgStartSpeed=glm(Result ~ StartSpeed, data = Kershaw, family = binomial(link=logit))
summary(rgStartSpeed)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgStartSpeed, test = "Chisq")

rgEndSpeed=glm(Result ~ EndSpeed, data = Kershaw, family = binomial(link=logit))
summary(rgEndSpeed)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgEndSpeed, test = "Chisq")

rgHDev=glm(Result ~ HDev, data = Kershaw, family = binomial(link=logit))
summary(rgHDev)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgHDev, test = "Chisq")

rgVDev=glm(Result ~ VDev, data = Kershaw, family = binomial(link=logit))
summary(rgVDev)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgVDev, test = "Chisq")


rgHPos=glm(Result ~ HPos, data = Kershaw, family = binomial(link=logit))
summary(rgHPos)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgHPos, test = "Chisq")

rgVPos=glm(Result ~ VPos, data = Kershaw, family = binomial(link=logit))
summary(rgVPos)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgVPos, test = "Chisq")


rgZone=glm(Result ~ Zone, data = Kershaw, family = binomial(link=logit))
summary(rgZone)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgZone, test = "Chisq")

rgNasty=glm(Result ~ Nasty, data = Kershaw, family = binomial(link=logit))
summary(rgNasty)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgNasty, test = "Chisq")

rgCount=glm(Result ~ Count, data = Kershaw, family = binomial(link=logit))
summary(rgCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgCount, test = "Chisq")

rgOuts=glm(Result ~ Outs, data = Kershaw, family = binomial(link=logit))
summary(rgOuts)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgOuts, test = "Chisq")

rgbatter=glm(Result ~ Batter, data = Kershaw, family = binomial(link=logit))
summary(rgbatter)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgbatter, test = "Chisq")

rgInning=glm(Result ~ Inning, data = Kershaw, family = binomial(link=logit))
summary(rgInning)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgInning, test = "Chisq")

rgBallCount=glm(Result ~ BallCount, data = Kershaw, family = binomial(link=logit))
summary(rgBallCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgBallCount, test = "Chisq")

rgStrikeCount=glm(Result ~ StrikeCount, data = Kershaw, family = binomial(link=logit))
summary(rgStrikeCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgStrikeCount, test = "Chisq")

rgABEvent=glm(Result ~ ABEvent, data = Kershaw, family = binomial(link=logit))
summary(rgABEvent)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgABEvent, test = "Chisq")


rgBatterNumber=glm(Result ~ BatterNumber, data = Kershaw, family = binomial(link=logit))
summary(rgBatterNumber)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgBatterNumber, test = "Chisq")

rgtime=glm(Result ~ Time, data = Kershaw, family = binomial(link=logit))
summary(rgtime)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgtime, test = "Chisq")

rgOutcome=glm(Result ~ Outcome, data = Kershaw, family = binomial(link=logit))
summary(rgOutcome)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgOutcome, test = "Chisq")

rgp=glm(Result ~ PitchType_Regrouped, data = Kershaw, family = binomial(link=logit))
summary(rgp)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgp, test = "Chisq")


rgsp=glm(Result ~ StartSpeed_categorica, data = Kershaw, family = binomial(link=logit))
summary(rgsp)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgsp, test = "Chisq")



# modelo completo con las variables con significacncia según el anova
modelo_completo <- glm(Result ~ Swing + StartSpeed+EndSpeed+VDev+Outs+ PitchType + Zone  + BatterHand + HPos + VPos  , data = Kershaw, family = "binomial"(link=logit))
summary(modelo_completo) # AIC: 2573.6


#modelos step aic
modelo_step <- stepAIC(modelo_completo, direction ="both")  # "both"
summary(modelo_step) # AIC: 2566

modelo_step1 <- stepAIC(modelo_completo, direction ="backward")  # "backward" 
summary(modelo_step1) # AIC: 2566

modelo_step2 <- stepAIC(modelo_completo, direction ="forward")  # "forward" 
summary(modelo_step2) # AIC: 2573.6

modelobi<-glm(Result ~Swing + Zone, data=Kershaw, family = "binomial"(link=logit))
summary(modelobi) # AIC: 2988.5

modelito<-glm(Result ~ Swing + Zone+ PitchType+VPos, data=Kershaw, family = "binomial"(link=logit))
summary(modelito) # AIC: 2566.7

modelito1<-glm(Result ~ Swing + Zone+VPos, data=Kershaw, family = "binomial"(link=logit))
summary(modelito1) # AIC: 2590.9


# probemos con algunas interacciones
modelo_interacciones<-glm(Result ~ Swing * PitchType + Zone * BatterHand + StartSpeed  + VDev + Outs + HPos + VPos ,data = Kershaw,family = binomial(link = "logit"))
summary(modelo_interacciones) # AIC: 2540.8

# modelos step aic
modelo_pasos <- stepAIC(modelo_interacciones, direction ="both")  # "both"
summary(modelo_pasos) # AIC: 2532.6

modelo_pasos1 <- stepAIC(modelo_interacciones, direction ="backward")  # "backward"
summary(modelo_pasos1) # AIC: 2532.6

modelo_pasos2 <- stepAIC(modelo_interacciones, direction ="forward")  # "forward"
summary(modelo_pasos2) # AIC: 2540.8


# dividimos el cjto de datos
set.seed(123)  # para reproducibilidad
n <- nrow(Kershaw)
indices <- sample(1:n)

train_index <- indices[1:round(0.7 * n)]
val_index   <- indices[(round(0.7 * n) + 1):(round(0.85 * n))]
test_index  <- indices[(round(0.85 * n) + 1):n]

train_data <- Kershaw[train_index, ]
val_data   <- Kershaw[val_index, ]
test_data  <- Kershaw[test_index, ]


# Crear lista de modelos para evaluar
modelos <- list(
  modelo_completo <- glm(Result ~ Swing + StartSpeed+EndSpeed+VDev+Outs+ PitchType + Zone  + BatterHand + HPos + VPos , data = train_data, family = "binomial"(link=logit)),
  
  modelo_step = stepAIC(modelo_completo, direction = "both"),
  
  modelo_step1 = stepAIC(modelo_completo, direction = "backward"),
  
  modelo_step2 = stepAIC(modelo_completo, direction = "forward"),
  
  modelobi = glm(Result ~ Swing + Zone, data = train_data, family = "binomial"(link = "logit")),
  
  modelito = glm(Result ~ Swing + Zone + PitchType + VPos , 
                 data = train_data, family = "binomial"(link = "logit")),
  
  modelito1 = glm(Result ~ Swing + Zone + VPos , data = train_data, family = "binomial"(link = "logit")),
  
  modelo_interaccion = glm(Result ~ Swing * PitchType + Zone * BatterHand + StartSpeed + EndSpeed + 
                             VDev + Outs + HPos + VPos , data = train_data, 
                           family = binomial(link = "logit")),
  
  modelo_pasos = stepAIC(modelo_interaccion, direction = "both"),
  
  modelo_pasos1 = stepAIC(modelo_interaccion, direction = "backward"),
  
  modelo_pasos2 = stepAIC(modelo_interaccion, direction = "forward")
)

# Crear un dataframe vacío para almacenar los resultados (AIC, AUC, R2)
resultados <- data.frame(Modelo = character(), AIC = numeric(), AUC = numeric(), stringsAsFactors = FALSE)

# Evaluar cada modelo
for(i in 1:length(modelos)) {
  modelo <- modelos[[i]]
  
  # Predicción en el conjunto de validación
  probs_val <- predict(modelo, newdata = val_data, type = "response")
  
  # Calcular la curva ROC y AUC
  pred_val <- prediction(probs_val, val_data$Result)
  perf_val <- performance(pred_val, "tpr", "fpr")
  
  # Calcular el AUC
  auc <- performance(pred_val, "auc")@y.values[[1]]
  
  # Calcular AIC del modelo
  aic <- AIC(modelo)
  
  # Almacenar el resultado en el dataframe
  resultados <- rbind(resultados, data.frame(Modelo = names(modelos)[i], AIC = aic, AUC = auc))
  
  # Graficar la curva ROC para cada modelo
  plot(perf_val, main = paste("Curva ROC: ", names(modelos)[i]), col = "blue")
  abline(a = 0, b = 1, lty = 2)
}

# Mostrar los resultados
print(resultados)





# Unir los datos de train y validation
train_val_data <- rbind(train_data, val_data)

# Modelo según AIC: modelo_pasos
modelo_pasos_final <- glm(Result ~ Swing * PitchType + Zone * BatterHand + StartSpeed + EndSpeed + VDev + Outs + HPos + VPos, 
                          data = train_val_data, family = binomial(link = "logit"))
summary(modelo_pasos_final)


# Predicciones para cada modelo en el test set
probs_pasos_test <- predict(modelo_pasos_final, newdata = test_data, type = "response")

# Calcular las predicciones y performance en el test set
pred_pasos_test <- prediction(probs_pasos_test, test_data$Result)

# Calcular AUC
auc_pasos_test <- performance(pred_pasos_test, "auc")@y.values[[1]]

# Graficar curvas ROC
plot(performance(pred_pasos_test, "tpr", "fpr"), col = "blue", main = "Curva ROC en el Test Set", lwd = 2, 
     xlab = "Tasa de Falsos Positivos", ylab = "Tasa de Verdaderos Positivos")

# Añadir la línea diagonal (ROC aleatoria)
abline(a = 0, b = 1, lty = 2)




# Establecer un umbral de corte (por ejemplo, 0.5)
umbral <- 0.5

# Convertir las probabilidades a clases (1 o 0)
predicciones_clase <- ifelse(probs_pasos_test > umbral, "Pos", "Neg")

# Calcular la matriz de confusión
confusion_matrix <- table(Predicted = predicciones_clase, Actual = test_data$Result)

# Mostrar la matriz de confusión
print(confusion_matrix)


########################################################### ¿DATOS BALANCEADOS?

# Ver la distribución de la variable 'Result'
table(Kershaw$Result)

# Ver proporciones
prop.table(table(Kershaw$Result))

# Realizar undersampling para balancear la clase 'result'
set.seed(123)  # Para reproducibilidad

# Realizamos undersampling, eligiendo la estrategia 'under'
balance_kershaw <- ovun.sample(Result ~ ., data = Kershaw, method = "under", N = 2500)$data

# Ver cuántas observaciones hay después del undersampling
table(balance_kershaw$Result)

# Gráfico de barras para la variable 'Result'
ggplot(mini_kershaw, aes(x = Result)) + 
  geom_bar(fill = "red") + 
  labs(title = "Distribución de Result", x = "Result", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


# Ver la distribución de la variable 'batterhand'
table(mini_kershaw$BatterHand)

# Realizar undersampling para balancear la clase 'batterhand'
set.seed(123)  # Para reproducibilidad

# Realizamos undersampling, eligiendo la estrategia 'under'
mini_kershaw <- ovun.sample(BatterHand ~ ., data = balance_kershaw, method = "under", N = 1050)$data

# Ver cuántas observaciones hay después del undersampling
table(mini_kershaw$BatterHand)

# Gráfico de barras para la variable 'BatterHand'
ggplot(mini_kershaw, aes(x = BatterHand)) + 
  geom_bar(fill = "lightsalmon") + 
  labs(title = "Distribución de BatterHand", x = "BatterHand", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


## BALANCEADO 
str(mini_kershaw)

# dividimos el cjto de datos
set.seed(123)  # para reproducibilidad
n <- nrow(mini_kershaw)
indices <- sample(1:n)

train_index <- indices[1:round(0.7 * n)]
val_index   <- indices[(round(0.7 * n) + 1):(round(0.85 * n))]
test_index  <- indices[(round(0.85 * n) + 1):n]

train_data <- Kershaw[train_index, ]
val_data   <- Kershaw[val_index, ]
test_data  <- Kershaw[test_index, ]

# Unir los datos de train y validation
train_val_data <- rbind(train_data, val_data)

# Modelo según AIC: modelo_pasos
modelo_pasos_final <- glm(Result ~ Swing * PitchType + Zone * BatterHand + StartSpeed  + VDev + Outs + HPos + VPos, 
                          data = train_val_data, family = binomial(link = "logit"))
summary(modelo_pasos_final)


# Predicciones para cada modelo en el test set
probs_pasos_test <- predict(modelo_pasos_final, newdata = test_data, type = "response")

# Calcular las predicciones y performance en el test set
pred_pasos_test <- prediction(probs_pasos_test, test_data$Result)

# Calcular AUC
auc_pasos_test <- performance(pred_pasos_test, "auc")@y.values[[1]] # 0.8945869

# Graficar curvas ROC
plot(performance(pred_pasos_test, "tpr", "fpr"), col = "blue", main = "Curva ROC en el Test Set", lwd = 2, 
     xlab = "Tasa de Falsos Positivos", ylab = "Tasa de Verdaderos Positivos")

# Añadir la línea diagonal (ROC aleatoria)
abline(a = 0, b = 1, lty = 2)

# Establecer un umbral de corte (por ejemplo, 0.5)
umbral <- 0.5

# Convertir las probabilidades a clases (1 o 0)
predicciones_clase <- ifelse(probs_pasos_test > umbral, "Pos", "Neg")

# Calcular la matriz de confusión
confusion_matrix <- table(Predicted = predicciones_clase, Actual = test_data$Result)

# Mostrar la matriz de confusión
print(confusion_matrix)


################################################################################
#                              PRÁCTICA 3
# REGRESIÓN LOGÍSTICA MULTINOMIAL Y ORDINAL; REGRESIÓN DE POISSON Y BINOMIAL NEGATIVA
################################################################################

# Aplica un modelo de regresión logística multinomial u ordinal en 
# alguna de las variables de tu base de datos, o aplica un modelo GLM de 
# tipo Poisson o binomial negativo para recuentos. Si tus datos no tienen 
# recuentos, puedes simular una nueva variable Luego, tendrías que inventarte el problema al que se referirían estos 
# datos para describir las interpretaciones. 
# ❖ Recuerda la importancia de establecer una buena pregunta de 
# investigación e interpretar los resultados en base a la misma. También 
# puedes llevar a cabo predicciones que apoyen o te ayuden con esta 
# interpretación. 


# Modelo nulo: solo el intercepto
modelo_nulo <- glm(Class ~ 1, data = Kershaw, family = binomial)



### MODELOS UNIVARIANTES

rgswing=glm(Class ~ Swing, data = Kershaw, family = binomial(link=logit))
summary(rgswing)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgswing, test = "Chisq")


rgbatterhand=glm(Class ~ BatterHand, data = Kershaw, family = binomial(link=logit))
summary(rgbatterhand)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgbatterhand, test = "Chisq")

rgoutcome=glm(Class ~ Outcome, data = Kershaw, family = binomial(link=logit))
summary(rgoutcome) # sobreajuste a mis datos

rgPitchType=glm(Class ~ PitchType, data = Kershaw, family = binomial(link=logit))
summary(rgPitchType)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgPitchType, test = "Chisq")

rgInningSide=glm(Class ~ InningSide, data = Kershaw, family = binomial(link=logit))
summary(rgInningSide)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgInningSide, test = "Chisq")

rgStartSpeed=glm(Class ~ StartSpeed, data = Kershaw, family = binomial(link=logit))
summary(rgStartSpeed)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgStartSpeed, test = "Chisq")

rgEndSpeed=glm(Class ~ EndSpeed, data = Kershaw, family = binomial(link=logit))
summary(rgEndSpeed)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgEndSpeed, test = "Chisq")

rgHDev=glm(Class ~ HDev, data = Kershaw, family = binomial(link=logit))
summary(rgHDev)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgHDev, test = "Chisq")

rgVDev=glm(Class ~ VDev, data = Kershaw, family = binomial(link=logit))
summary(rgVDev)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgVDev, test = "Chisq")


rgHPos=glm(Class ~ HPos, data = Kershaw, family = binomial(link=logit))
summary(rgHPos)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgHPos, test = "Chisq")

rgVPos=glm(Class ~ VPos, data = Kershaw, family = binomial(link=logit))
summary(rgVPos)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgVPos, test = "Chisq")


rgZone=glm(Class ~ Zone, data = Kershaw, family = binomial(link=logit))
summary(rgZone)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgZone, test = "Chisq")

rgNasty=glm(Class ~ Nasty, data = Kershaw, family = binomial(link=logit))
summary(rgNasty)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgNasty, test = "Chisq")

rgCount=glm(Class ~ Count, data = Kershaw, family = binomial(link=logit))
summary(rgCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgCount, test = "Chisq")

rgOuts=glm(Class ~ Outs, data = Kershaw, family = binomial(link=logit))
summary(rgOuts)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgOuts, test = "Chisq")

rgbatter=glm(Class ~ Batter, data = Kershaw, family = binomial(link=logit))
summary(rgbatter)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgbatter, test = "Chisq")

rgInning=glm(Class ~ Inning, data = Kershaw, family = binomial(link=logit))
summary(rgInning)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgInning, test = "Chisq")

rgBallCount=glm(Class ~ BallCount, data = Kershaw, family = binomial(link=logit))
summary(rgBallCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgBallCount, test = "Chisq")

rgStrikeCount=glm(Class ~ StrikeCount, data = Kershaw, family = binomial(link=logit))
summary(rgStrikeCount)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgStrikeCount, test = "Chisq")

rgABEvent=glm(Class ~ ABEvent, data = Kershaw, family = binomial(link=logit))
summary(rgABEvent)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgABEvent, test = "Chisq")


rgBatterNumber=glm(Class ~ BatterNumber, data = Kershaw, family = binomial(link=logit))
summary(rgBatterNumber)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgBatterNumber, test = "Chisq")

rgtime=glm(Class ~ Time, data = Kershaw, family = binomial(link=logit))
summary(rgtime)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgtime, test = "Chisq")

rgOutcome=glm(Class ~ Outcome, data = Kershaw, family = binomial(link=logit))
summary(rgOutcome)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgOutcome, test = "Chisq")

rgp=glm(Class ~ PitchType_Regrouped, data = Kershaw, family = binomial(link=logit))
summary(rgp)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgp, test = "Chisq")


rgsp=glm(Class ~ StartSpeed_categorica, data = Kershaw, family = binomial(link=logit))
summary(rgsp)
# Comparación con test de razón de verosimilitudes
anova(modelo_nulo, rgsp, test = "Chisq")




# Dividir el dataset en Train (70%), Validation (15%) y Test (15%)
set.seed(42)  # Fijar semilla para reproducibilidad

# Crear índices aleatorios para la división
n <- nrow(Kershaw)
train_idx <- sample(1:n, size = 0.7 * n)
val_test_idx <- setdiff(1:n, train_idx)
val_idx <- sample(val_test_idx, size = 0.5 * length(val_test_idx))
test_idx <- setdiff(val_test_idx, val_idx)

# Asignar los subconjuntos a los diferentes conjuntos
train_data <- Kershaw[train_idx, ]
val_data <- Kershaw[val_idx, ]
test_data <- Kershaw[test_idx, ]

# Ajustar el modelo multinomial en el conjunto de entrenamiento
modelo_multivariable <- multinom(Class ~ PitchType + Swing + Zone + StartSpeed + EndSpeed + VDev + HPos + VPos + Outs, data = train_data)
summary(modelo_multivariable) 
# Residual Deviance: 2448.814 
# AIC: 2496.814 

# Realizar selección paso a paso basada en el AIC
modelo_stepwise <- stepAIC(modelo_multivariable, direction = "both", trace = FALSE)
summary(modelo_stepwise)
# Residual Deviance: 2452.778 
# AIC: 2488.778 

# Predecir probabilidades en el conjunto de test
probs_test <- predict(modelo_stepwise, newdata = test_data, type = "probs")

# Si es un problema de clasificación binaria (por ejemplo, "Neg" vs "Pos")
# Convertir probabilidades a clases según un umbral (por ejemplo, 0.5)
umbral <- 0.5
predicciones_clase_test <- ifelse(probs_test[, "Pos"] > umbral, "Pos", "Neg")

# Calcular la matriz de confusión
confusion_matrix_test <- table(Predicted = predicciones_clase_test, Actual = test_data$Class)
print(confusion_matrix_test)

# Evaluar el AUC
library(ROCR)
pred_test <- prediction(probs_test[, "Pos"], test_data$Class)
auc_test <- performance(pred_test, "auc")@y.values[[1]]
print(paste("AUC en el conjunto de test:", auc_test))

# Graficar la curva ROC
plot(performance(pred_test, "tpr", "fpr"), col = "blue", main = "Curva ROC en el conjunto de test", lwd = 2, 
     xlab = "Tasa de Falsos Positivos", ylab = "Tasa de Verdaderos Positivos")

# Añadir la línea diagonal (ROC aleatoria)
abline(a = 0, b = 1, lty = 2)






# Predecir probabilidades en el conjunto de validación
probs_val <- predict(modelo_stepwise, newdata = val_data, type = "probs")

# Crear la predicción para evaluación del AUC
pred_val <- prediction(probs_val, val_data$Class)

# Calcular AUC
auc_val <- performance(pred_val, "auc")@y.values[[1]]
print(paste("AUC en el conjunto de validación:", auc_val))

# Unir entrenamiento y validación para reentrenar el modelo
train_val_data <- rbind(train_data, val_data)

# Ajustar el modelo en el conjunto combinado (train + validation)
modelo_final <- multinom(Class ~ PitchType + Swing + Zone + StartSpeed + EndSpeed + VDev + HPos + VPos + Outs, data = train_val_data)
modelo_final<- stepAIC(modelo_final, direction = "both", trace = FALSE)
# Predecir en el conjunto de prueba
probs_test <- predict(modelo_final, newdata = test_data, type = "probs")

# Crear la predicción para evaluación del AUC en el conjunto de prueba
pred_test <- prediction(probs_test, test_data$Class)

# Calcular AUC en el conjunto de prueba
auc_test <- performance(pred_test, "auc")@y.values[[1]]
print(paste("AUC en el conjunto de prueba:", auc_test))

# Obtener las predicciones finales para el conjunto de prueba
predicciones_test <- predict(modelo_final, newdata = test_data)

# Calcular la matriz de confusión
confusion_matrix <- table(Predicted = predicciones_test, Actual = test_data$Class)

# Mostrar la matriz de confusión
print(confusion_matrix)




zone_swing_data <- Kershaw %>%
  group_by(Zone) %>%
  summarise(
    swing_rate = mean(Swing == "Yes"),
    total = n()
  )

ggplot(zone_swing_data, aes(x = factor(Zone), y = 1, fill = swing_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Swing rate por zona",
    x = "Zona",
    y = "",
    fill = "Swing rate"
  ) +
  theme_minimal()
