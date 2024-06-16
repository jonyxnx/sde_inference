library(dplyr)
library(ggplot2)

# Cargar datos desde un archivo CSV
data <- read.csv("BASE.csv")

# Eliminar filas con valores NA
data <- na.omit(data)

# Seleccionar las columnas relevantes
data <- data[, c("Fecha", "Cierre")]

# Invertir el orden de las filas
data <- data[rev(1:nrow(data)), ]

# Ver la estructura de los datos
str(data)

# Graficar la columna "Cierre"
plot(data[, "Cierre"])

# Fecha base para la conversión de fechas
fecha_base <- as.Date("07.01.2016", format = "%d.%m.%Y")

# Función para convertir la fecha a años desde la fecha base
convertir_fecha <- function(fecha) {
  # Convertir la fecha a objeto Date
  fecha_date <- as.Date(fecha, format = "%d.%m.%Y")
  
  # Calcular la diferencia en meses desde la fecha base
  months_since_base <- as.numeric(difftime(fecha_date, fecha_base, units = "weeks")) / 4.345
  
  # Convertir la diferencia en meses a años
  years_since_base <- months_since_base / 12
  
  return(years_since_base)
}

# Aplicar la función para convertir fechas a años desde la fecha base
data$Años_desde_base <- sapply(data$Fecha, convertir_fecha)

# Agregar una columna con los índices de tiempo
data <- data %>% mutate(indice = sapply(Fecha, convertir_fecha))

# Extraer los años y tasas de interés
years <- data$Años_desde_base
rates <- data[, "Cierre"]/100

# Crear un dataframe con los años y tasas
data <- data.frame(Year = years, Rate = rates)

# Graficar las tasas de interés a corto plazo
ggplot(data, aes(x = Year, y = Rate)) +
  geom_line(color = "blue", size = 1) +         
  geom_point(color = "red", size = 1) +         
  labs(title = "Tasas de Interés a Corto Plazo", 
       x = "Años",                             
       y = "Tasa") +                     
  theme_minimal() +                          
  theme(plot.title = element_text(hjust = 0.5)) 

# Función para interpolar theta
THETA <- function(t, tiempos, theta){  
  # Calcular el intervalo de tiempo en el que se encuentra t
  indice_inferior <- findInterval(t, tiempos)
  
  # Si t no está dentro del intervalo donde theta está definida, retornar 0
  if (indice_inferior == length(tiempos) || indice_inferior == 0) {
    return(0)
  }
  
  indice_superior <- indice_inferior + 1
  
  # Interpolar el valor de theta
  valor <- theta[indice_inferior] + (t - tiempos[indice_inferior]) * (theta[indice_superior] - theta[indice_inferior]) / (tiempos[indice_superior] - tiempos[indice_inferior])
  
  return(valor)
}

# Función para estimar theta y sigma en un modelo continuo completo
estimarContinuoCompleto <- function(tasas, tiempos){
  # Función interna para calcular las tasas forward con el modelo de Ho-Lee
  calculate_ho_lee_forward_rates <- function(spot_rates) {
    n <- length(spot_rates)
    forward_rates <- numeric(n - 1)
    
    for (i in 1:(n - 1)) {
      forward_rate <- ((1 + spot_rates[i + 1])^(i + 1) / (1 + spot_rates[1])^(1))^(1/i) - 1
      forward_rates[i] <- forward_rate
    }
    
    return(forward_rates)
  }
  
  # Calcular las tasas forward
  f <- calculate_ho_lee_forward_rates(tasas)
  
  # Calcular las diferencias de las tasas
  differences <- diff(tasas)
  # Sumar las diferencias al cuadrado
  sum_squared_differences <- sum(differences^2)
  # Calcular sigma usando la variación total
  sigma <- sqrt(sum_squared_differences / tiempos[length(tiempos)])
  # Calcular la derivada de la tasa forward
  df <- diff(tasas) / (tiempos[3] - tiempos[2])
  # Construir theta con los valores de las derivadas de la tasa forward
  theta <- df + (sigma^2) * (tiempos[-c(1)]^2) / 2
  
  return(list(theta, sigma))
}

# Estimar theta y sigma para los datos
estimacion <- estimarContinuoCompleto(rates, years)
theta_gorro <- unname(estimacion[[1]])
sigma_gorro <- unname(estimacion[[2]])
print(sigma_gorro)

# Función para simular trayectorias usando el modelo de Euler
SIMULAR <- function(r0, sigma_gorro, theta_gorro, tiempos, n, trayectorias){
  T <- tiempos[length(tiempos)]
  dt <- T / n
  
  r <- matrix(0, nrow = trayectorias, ncol = n + 1)
  r[, 1] <- r0
  
  for (j in 1:trayectorias) {
    for (i in 1:n) {
      dr <- THETA(i * dt, tiempos, theta_gorro)[1] * dt + sigma_gorro * sqrt(dt) * rnorm(1)
      r[j, i + 1] <- r[j, i] + dr
    }
  }
  
  # Crear dataframe con las trayectorias simuladas
  time <- seq(0, T, length.out = n + 1)
  df <- data.frame(time = rep(time, trayectorias), r = c(t(r)))
  
  # Graficar las trayectorias
  ggplot(df, aes(x = time, y = r, group = factor(rep(1:trayectorias, each = n + 1)))) +
    geom_line(color = 'blue', alpha = 0.5) +
    labs(x = "Tiempo (años)", y = "Tasa de interés", title = "Simulación modelo ajustado") +
    theme_minimal()
}

# Función para interpolar theta
THETA <- function(t, tiempos, theta){  
  indice_inferior <- findInterval(t, tiempos) # Calculamos el intervalo de tiempo en el que esta t
  
  if (indice_inferior == length(tiempos) || indice_inferior==0) {
    return(0)  # Si no está dentro del intervalo donde theta está definida, la hacemos 0
  }
  
  indice_superior <- indice_inferior + 1
  
  # Calculamos el valor de theta interpolando entre el valor posterior y el valor anterior
  valor <- theta[indice_inferior] + (t - tiempos[indice_inferior]) * (theta[indice_superior] - theta[indice_inferior]) / (tiempos[indice_superior] - tiempos[indice_inferior])
  
  return(valor)
}


# Establecer la semilla para la reproducibilidad
set.seed(123)

# Ejecutar la simulación con el modelo ajustado
SIMULAR(rates[1], sigma_gorro, theta_gorro, years, length(rates), 10)

# Iterar para estimar sigma en cada iteración
sigmas <- numeric(1000)

for (i in 1:1000) {
  subset <- head(data, i + 5)
  sigmas[i] <- estimarContinuoCompleto(subset$Rate, subset$Year)[[2]]
}

# Crear un dataframe con los resultados
sigmas <- data.frame(Iteraciones = 1:length(sigmas), Sigma = sigmas)

# Crear el gráfico de convergencia de sigma
ggplot(sigmas, aes(x = Iteraciones, y = Sigma)) +
  geom_line(color = "red", size = 1.5) + 
  geom_point(color = "red", size = 1.5) + 
  ggtitle("Convergencia de Sigma") +
  xlab("Iteraciones") +
  ylab("Sigma") +
  scale_y_continuous(limits = c(0, 0.04)) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calcular y mostrar los cuantiles al 2.5% y al 97.5%
cuantiles <- print(quantile(sigmas$Sigma, probs = c(0.025, 0.975)))
print(paste("El cuantil al 2.5% es:", cuantiles[1]))
print(paste("El cuantil al 97.5% es:", cuantiles[2]))

