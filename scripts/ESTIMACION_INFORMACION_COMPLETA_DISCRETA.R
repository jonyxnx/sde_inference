library(ggplot2)

# Función para estimar theta y sigma en un modelo discreto completo
estimarDiscretoCompleto <- function(tasas, tiempos){
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
  
  n <- length(tasas)  
  
  # Calcular las sumas necesarias para estimar sigma
  sum_2i_1 <- sum((2*tiempos - 1)^2) # Suma 2i-1
  sum_diff <- sum((diff(tasas[-length(tasas)]) - diff(f))^2) # Suma de r_i - f(0,i)
  parte_raiz <- (sum_diff * sum_2i_1 + n^2) # Parte que va dentro de la raíz
  sigma <- sqrt(2) * sqrt((sqrt(parte_raiz) - n) / sum_2i_1) # Cálculo de sigma
  
  # Calcular theta
  df <- diff(tasas)/(tiempos[3]-tiempos[2]) # Calcular la derivada de la tasa forward
  theta <- df + (sigma^2) * (tiempos[-c(1)]^2) / 2 # Construir theta con los valores de las derivadas de la tasa forward
  
  return(list(theta, sigma))
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


# PRUEBA

# Función para simular trayectorias usando el modelo de Euler
SIMULAR_EULER <- function(sigma, T, trayectorias, n){
  
  # Función para la curva forward
  forward_curve <- function(x) {
    return(0.05+ dnorm(x,3,2)/8)
  }
  
  # Función theta
  dt <- T / n
  
  theta <- function(t) {
    df_dt <- (forward_curve(t + dt) - forward_curve(t)) / dt
    return(df_dt + 0.5 * sigma^2 * t)
  }
  
  # Simulación de trayectorias
  r <- matrix(0, nrow = trayectorias, ncol = n + 1)
  r[, 1] <- forward_curve(0)
  
  for (j in 1:trayectorias) {
    for (i in 1:n) {
      dr <- theta(i * dt) * dt + sigma * sqrt(dt) * rnorm(1)
      r[j, i + 1] <- r[j, i] + dr
    }
  }
  
  # Gráfico de las trayectorias
  time <- seq(0, T, length.out = n + 1)
  df <- data.frame(time = rep(time, trayectorias), r = c(t(r)))
  p <- ggplot(df, aes(x = time, y = r, group = factor(rep(1:trayectorias, each = n + 1)))) +
    geom_line(color = 'blue', alpha = 0.5) +
    labs(x = "Tiempo (semanas)", y = "Tasa de interés", title = "Datos Simulados") +
    theme_minimal()
  print(p)
  return(df)
}

# Establecer la semilla para la reproducibilidad
set.seed(123)

# Ejecutar la simulación con el modelo de Euler
set_pruebaDiscreto <- SIMULAR_EULER(0.01, 140, 1, 140)

# Estimar theta y sigma
estimacion <- estimarDiscretoCompleto(set_pruebaDiscreto$r, set_pruebaDiscreto$time)
theta_gorro <- unname(estimacion[[1]])
sigma_gorro <- unname(estimacion[[2]])

# Iterar para estimar sigma en cada iteración
sigmas <- numeric(137)

for (i in 1:137){
  subset <- head(set_pruebaDiscreto, i + 3)
  sigmas[i] <- estimarDiscretoCompleto(subset$r, subset$time)[[2]]
}

# Crear dataframe con los resultados
sigmas <- data.frame(Iteraciones = 1:length(sigmas), Sigma = sigmas)

# Crear el gráfico de convergencia de sigma
ggplot(sigmas, aes(x = Iteraciones, y = Sigma)) +
  geom_line(color = "red", size = 1.5) + 
  geom_point(color = "red", size = 1.5) + 
  geom_hline(yintercept = 0.01, color = "blue", linetype = "dashed", size = 1) +
  ggtitle("Convergencia de Sigma") +
  xlab("Tiempo") +
  ylab("Sigma") +
  scale_y_continuous(limits = c(0, 0.02)) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.01))

# Calcular y mostrar los cuantiles al 2.5% y al 97.5%
cuantiles <- quantile(sigmas$Sigma, probs = c(0.025, 0.975))

print(paste("El cuantil al 2.5% es:", cuantiles[1]))
print(paste("El cuantil al 97.5% es:", cuantiles[2]))
