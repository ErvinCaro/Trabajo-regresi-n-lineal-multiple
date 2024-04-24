library(ggplot2)
library(dplyr)
library(tibble)
library(readxl)
library(openxlsx)
library(plotly)
library(tidyr)
library(gridExtra)
library(car)
library(olsrr)
library(lmtest)
library(nortest)


# Lee el archivo Excel
choose.files()
datos <- read_excel("C:/Users/juseb/Downloads/datos_carros.xlsx")

# Eliminar filas con valores en blanco (NA)
datos_carros <- datos[complete.cases(datos), ]

# Visualizar las primeras filas de los datos
head(datos_carros)

# Crear el gráfico interactivo de burbujas en 2D con colores según la transmisión
plot <- plot_ly(data = datos_carros, x = ~year, y = ~selling_price, 
                size = ~km_driven, color = ~transmission, 
                colors = c("Automatic" = "blue", "Manual" = "red"), 
                type = "scatter", mode = "markers",
                text = ~paste("Name: ", name, "<br>",
                              "Seller Type: ", seller_type, "<br>",
                              "Transmission: ", transmission, "<br>",
                              "Owner: ", owner, "<br>",
                              "Mileage: ", mileage, "<br>",
                              "Engine: ", engine, "<br>",
                              "Max Power: ", max_power, "<br>",
                              "Torque: ", torque, "<br>",
                              "Seats: ", seats)) %>%
  layout(title = "Relación entre año, precio de venta y kilometraje",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Precio de venta"),
         hovermode = "closest")

# Mostrar el gráfico
plot

# Calcular la media para cada columna numérica
mean_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarise_all(mean)

#imprimir media
print("Media:")
print(mean_stats)

# Calcular la mediana para cada columna numérica
median_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarise_all(median)

#imprimir mediana
print("Mediana:")
print(median_stats)

# Calcular la desviación estándar para cada columna numérica
sd_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarise_all(sd)

# Imprimir desviacion estandar
print("Desviación estándar:")
print(sd_stats)

# Calcular el mínimo para cada columna numérica
min_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarise_all(min)

#Imprimir minimo
print("Mínimo:")
print(min_stats)

# Calcular el máximo para cada columna numérica
max_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarise_all(max)

#Imprimir maximo
print("Máximo:")
print(max_stats)

# Combinar todas las medidas resumidas en un solo dataframe
summary_combined <- bind_rows(mean_stats, median_stats, sd_stats, min_stats, max_stats, .id = "summary_type")

# Imprimir el dataframe combinado
print(summary_combined)

# Calcula las estadísticas resumidas para todas las variables numéricas en datos_clean
summary_stats <- datos_carros %>%
  select_if(is.numeric) %>%
  summarize_all(list(mean = mean, median = median, sd = sd, min = min, max = max))


#1.year (Año):
#-La media y la mediana están muy cerca, lo que sugiere una distribución simétrica de los años de los autos en tus datos.
#-La desviación estándar es relativamente baja, lo que indica que los años están relativamente concentrados alrededor de la media.
#-El rango de años es desde 1994 hasta 2020, lo que sugiere una amplia cobertura de años en tus datos.
#2.selling_price (Precio de venta):
#-La media y la mediana están bastante distantes, lo que sugiere una distribución sesgada hacia la derecha en los precios de venta.
#-La desviación estándar es alta, lo que indica una gran variabilidad en los precios de venta.
#-El rango de precios es desde 29999 hasta 10000000, lo que indica una amplia gama de precios de venta en tus datos.
#3.km_driven (Kilometraje recorrido):
#-La media y la mediana están bastante cercanas, lo que sugiere una distribución relativamente simétrica del kilometraje recorrido.
#-La desviación estándar es moderada, lo que indica una cierta variabilidad en el kilometraje recorrido.
#-El rango de kilómetros recorridos es desde 1 hasta 2360457, lo que sugiere una amplia gama de valores en tus datos.
#4.mileage (Consumo de combustible):
#-La media y la mediana están bastante distantes, lo que sugiere una distribución sesgada hacia la derecha en el consumo de combustible.
#-La desviación estándar es alta, lo que indica una gran variabilidad en el consumo de combustible.
#-El rango de consumo de combustible es desde 0 hasta 33922, lo que sugiere una amplia variación en los niveles de consumo.
#5.engine (Cilindrada del motor):
#-La media y la mediana están relativamente cercanas, lo que sugiere una distribución simétrica de la cilindrada del motor.
#-La desviación estándar es moderada, lo que indica una cierta variabilidad en la cilindrada del motor.
#-El rango de cilindrada del motor es desde 624 hasta 3604, lo que indica una amplia gama de valores en tus datos.
#6.max_power (Potencia máxima):
#-La media y la mediana están bastante distantes, lo que sugiere una distribución sesgada hacia la derecha en la potencia máxima.
#-La desviación estándar es muy alta, lo que indica una gran variabilidad en la potencia máxima.
#-El rango de potencia máxima es desde 32.8 hasta 108495, lo que sugiere una amplia variación en los niveles de potencia.
#7.seats (Número de asientos):
#-La media y la mediana están cercanas, lo que sugiere una distribución simétrica del número de asientos.
#-La desviación estándar es baja, lo que indica que el número de asientos está relativamente concentrado alrededor de la media.
#-El rango del número de asientos es desde 2 hasta 14, lo que sugiere una amplia variación en el número de asientos en tus datos.

# Seleccionar solo las variables numéricas del conjunto de datos
datos_numericos <- datos_carros %>%
  select_if(is.numeric)

# Calcular la correlación de Pearson entre las variables numéricas
correlaciones <- cor(datos_numericos, use = "complete.obs", method = "pearson")

# Mostrar las correlaciones
correlaciones

# Tablas de frecuencia y gráficos de barras para variables categóricas
categorical_vars <- c("fuel", "seller_type", "transmission", "owner", "seats") 
for (var in categorical_vars) {
  freq_table <- table(datos_carros[[var]])
  cat(paste("Tabla de frecuencia para", var, ":\n"))
  print(freq_table)
  barplot(freq_table, main = paste("Gráfico de barras para", var))
}

# Gráficos de distribución para variables numéricas

# Histograma para selling_price
hist_plot_selling_price <- ggplot(datos_carros, aes(x = selling_price)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Histograma para selling_price", x = "selling_price", y = "Frecuencia")

print(hist_plot_selling_price)

# Diagrama de caja para selling_price
box_plot_selling_price <- ggplot(datos_carros, aes(y = selling_price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Diagrama de caja para selling_price", y = "selling_price")

print(box_plot_selling_price)

# Gráfico de densidad para selling_price
density_plot_selling_price <- ggplot(datos_carros, aes(x = selling_price)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Gráfico de densidad para selling_price", x = "selling_price", y = "Densidad")

print(density_plot_selling_price)

# Histograma para km_driven
hist_plot_km_driven <- ggplot(datos_carros, aes(x = km_driven)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Histograma para km_driven", x = "km_driven", y = "Frecuencia")

print(hist_plot_km_driven)

# Diagrama de caja para km_driven
box_plot_km_driven <- ggplot(datos_carros, aes(y = km_driven)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Diagrama de caja para km_driven", y = "km_driven")

print(box_plot_km_driven)

# Gráfico de densidad para km_driven
density_plot_km_driven <- ggplot(datos_carros, aes(x = km_driven)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Gráfico de densidad para km_driven", x = "km_driven", y = "Densidad")

print(density_plot_km_driven)

# Histograma para year
hist_plot_year <- ggplot(datos_carros, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histograma para year", x = "year", y = "Frecuencia")

print(hist_plot_year)

# Diagrama de caja para year
box_plot_year <- ggplot(datos_carros, aes(y = year)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Diagrama de caja para year", y = "year")

print(box_plot_year)

# Gráfico de densidad para year
density_plot_year <- ggplot(datos_carros, aes(x = year)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Gráfico de densidad para year", x = "year", y = "Densidad")

print(density_plot_year)

# Análisis de valores atípicos (outliers)
outliers <- datos_carros %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(Q1 = ~quantile(., probs = 0.25, na.rm = TRUE), 
                                      Q3 = ~quantile(., probs = 0.75, na.rm = TRUE)))) %>%
  mutate(across(everything(), ~ . - 1.5 * IQR(.) )) %>%
  rownames_to_column(var = "Variable")
outliers

# Comprobación de datos faltantes
missing_data <- datos_carros %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count")
head(missing_data)

# Ajustar un modelo de regresión lineal con todas las variables (1)
modelo_inicial <- lm(selling_price ~ ., data = datos_carros)

#Analisis de los boxplot
# Boxplot para year
boxplot_year <- ggplot(datos_carros, aes(x = factor(year), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Año", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Año") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot para km_diven
boxplot_km <- ggplot(datos_carros, aes(x = factor(km_driven), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Kilometraje", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Kilometraje")

# Boxplot para fuel
boxplot_fuel <- ggplot(datos_carros, aes(x = factor(fuel), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Tipo de Combustible", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Tipo de Combustible")

# Boxplot para seller_type
boxplot_seller_type <- ggplot(datos_carros, aes(x = factor(seller_type), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Tipo de Vendedor", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Tipo de Vendedor")

# Boxplot para transmission
boxplot_transmission <- ggplot(datos_carros, aes(x = factor(transmission), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Tipo de Transmisión", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Tipo de Transmisión")

# Boxplot para owner
boxplot_owner <- ggplot(datos_carros, aes(x = factor(owner), y = selling_price)) +
  geom_boxplot() +
  labs(x = "Dueño Anterior", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Dueño Anterior")

# Boxplot para mileage
boxplot_mileage <- ggplot(datos_carros, aes(x = mileage, y = selling_price)) +
  geom_boxplot() +
  labs(x = "Kilometraje", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Kilometraje")

# Boxplot para engine
boxplot_engine <- ggplot(datos_carros, aes(x = engine, y = selling_price)) +
  geom_boxplot() +
  labs(x = "Tamaño del Motor", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Tamaño del Motor")

# Boxplot para max_power
boxplot_max_power <- ggplot(datos_carros, aes(x = max_power, y = selling_price)) +
  geom_boxplot() +
  labs(x = "Potencia Máxima", y = "Precio de Venta") +
  ggtitle("Boxplot: Precio de Venta por Potencia Máxima")

# Visualizar los boxplots
grid.arrange(boxplot_year, boxplot_km, boxplot_fuel, boxplot_seller_type,
             boxplot_transmission, boxplot_owner, boxplot_mileage,
             boxplot_engine, boxplot_max_power,
             ncol = 3)


#Prueba de significancia del modelo 
anova(modelo_inicial)

#a continuacion validaremos que el modelo en su gran mayoria es expresado por la variable "name" 
#la cual no tendremos presente en los siguientes modelos 
# Modelo inicial con solo la intersección (2)

modelo_inicial <- lm(selling_price ~ 1, data = datos_carros)

# Actualizar el modelo añadiendo las variables predictora name 
modelo_inicial <- update(modelo_inicial, . ~ . + name, data = datos_carros)

#Prueba de significancia del modelo 
anova(modelo_inicial)

#nos mostraria un R-Square y Adj. R-Square del 0.9844 y 0.9792 respectivamente 
summary(modelo_inicial)

#comenzamos a realizar modelos sin contar la variable name y torque
# Modelo inicial con solo la intersección (3)
modelo_inicial <- lm(selling_price ~ 1, data = datos_carros)

# Actualizar el modelo añadiendo las variables predictoras una por una 
# sacamos el nombre y torque
modelo_inicial <- update(modelo_inicial, . ~ . + year, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + km_driven, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + fuel, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + seller_type, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + transmission, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + owner, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + mileage, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + max_power, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + seats, data = datos_carros)

# Crear el diagrama de burbujas interactivo 
plot_ly(data = datos_carros, x = ~year, y = ~km_driven, size = ~selling_price, color = ~fuel, 
        text = paste("Año:", datos_carros$year, "<br>",
                     "Kilometraje Recorrido:", datos_carros$km_driven, "<br>",
                     "Combustible:", datos_carros$fuel, "<br>",
                     "Tipo de Vendedor:", datos_carros$seller_type, "<br>",
                     "Transmisión:", datos_carros$transmission, "<br>",
                     "Dueños:", datos_carros$owner, "<br>",
                     "Rendimiento:", datos_carros$mileage, "<br>",
                     "Potencia Máxima:", datos_carros$max_power, "<br>",
                     "Precio de Venta: $", round(datos_carros$selling_price, 2)), 
        hoverinfo = "text", mode = "markers") %>%
  layout(title = "Diagrama de Burbujas Interactivo",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Kilometraje Recorrido"),
         showlegend = TRUE)

#Prueba de significancia del modelo 
anova(modelo_inicial)

#eliminamos las variables no significativas 

# Modelo inicial con solo la intersección (4)
modelo_inicial <- lm(selling_price ~ 1, data = datos_carros)

# Actualizar el modelo añadiendo las variables predictoras una por una 

# sacando las no significantes del modelo para realizar un 3 modelo
modelo_inicial <- update(modelo_inicial, . ~ . + year, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + km_driven, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + fuel, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + seller_type, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + transmission, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + owner, data = datos_carros)
modelo_inicial <- update(modelo_inicial, . ~ . + seats, data = datos_carros)

# comparamos los resultados de diferentes modelos 
ols_step_best_subset(modelo_inicial)

#con base a lo anterior nos quedaremos con el modelo que no incluye name, torque, milag
#mostrar resumen del intento
summary(modelo_inicial)

# Eliminamos Outliers

outlierTest(modelo_inicial)
id<- ols_plot_resid_lev(modelo_inicial)
options(max.print = 4000)
influence.measures(modelo_inicial)

# Calcula los residuos del modelo
residuos <- resid(modelo_inicial)

# Calcula los residuos estandarizados
residuos_estandarizados <- rstandard(modelo_inicial)

# Define un umbral para identificar outliers basados en residuos estandarizados
umbral <- 2  # Puedes ajustar este valor según tus necesidades

# Encuentra las observaciones que superan el umbral
outliers <- which(abs(residuos_estandarizados) > umbral)

# Elimina las observaciones con outliers del conjunto de datos
datos_sin_outliers <- datos_carros[-outliers, ]

# Verifica la cantidad de observaciones eliminadas
cat("Número de outliers eliminados:", length(outliers), "\n")

# Verifica la nueva cantidad de observaciones en el conjunto de datos
cat("Número de observaciones restantes:", nrow(datos_sin_outliers), "\n")

# Gráfica los residuos sin outliers
plot(residuos[-outliers], main = "Residuos del modelo sin outliers", xlab = "Índice de observación", ylab = "Residuos")

# Grafica los residuos sin outliers
datos_sin_puntos <- lm(selling_price ~ year + km_driven + fuel + seller_type + transmission + owner + seats, data = datos_sin_outliers)
ols_plot_resid_lev(datos_sin_puntos)

#resumen sin outliers
summary(datos_sin_puntos)

# Evaluar la normalidad de los residuos
datos_residuales <-rstandard(datos_sin_puntos)
ad.test(datos_residuales)#mayor al significancia del modelo se aprueba la normalidad
qqPlot(datos_residuales) #prueba Q-Q


#shapiro test
# Ya que la muestra es "grande" haremos un subconjunto mas pequeño de muestras aleatorias
# Tomar una muestra aleatoria de los residuos si el tamaño de la muestra es mayor que 5000
if (length(residuos_estandarizados) > 5000) {
  set.seed(123) # establecer semilla para reproducibilidad
  residuos_muestra <- sample(residuos_estandarizados, 5000)
} else {
  residuos_muestra <- residuos_estandarizados
}
# Realizar la prueba de normalidad de Shapiro-Wilk en la muestra de residuos
shapiro.test(residuos_muestra)

# Evaluar la homocedasticidad de los residuos

library(lmtest)
plot(predict(datos_sin_puntos), resid(datos_sin_puntos), main = "Residuos vs. Valores ajustados")
abline(h = 0, col = "red")
bptest(datos_sin_puntos)  # Prueba de Breusch-Pagan valor p < (0.005) heterocedasticidad

# Evaluar la independencia de los residuos
acf(resid(datos_sin_puntos), main = "Función de autocorrelación de los residuos") # si los picos se salen "mucho" del margen, hay dependencia
dwtest(datos_sin_puntos)  # Prueba de Durbin-Watson p-value < 0,005


# Resumen del modelo para examinar los coeficientes estimados
summary(modelo_inicial)

# Coeficientes estimados
coeficientes <- coef(modelo_inicial)

# Filtrar los coeficientes relevantes para las variables predictoras
coeficientes_predictores <- coeficientes[!names(coeficientes) %in% c("(Intercept)")]

# Ordenar los coeficientes por valor absoluto para ver las variables más influyentes
coeficientes_ordenados <- coeficientes_predictores[order(abs(coeficientes_predictores), decreasing = TRUE)]

# Mostrar los coeficientes ordenados
print(coeficientes_ordenados)





