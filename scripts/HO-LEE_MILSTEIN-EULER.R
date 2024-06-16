library(ggplot2)

# Parámetros del modelo  
sigma <- 0.2  # Volatilidad
T <- 10     # Tiempo al vencimiento (años)
trayectorias <- 4  # Número de trayectorias simuladas
n <- 1000  # Número de pasos de tiempo


SIMULAR_EULER <- function(sigma, T, trayectorias, n){

  # Función para la curva forward
  forward_curve <- function(x) {
    return(0.15+ dgamma(x,3,2))
  }

  #Funcion theta
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
    labs(x = "Tiempo (años)", y = "Tasa de interés", title = "Simulación modelo de Ho-Lee por Euler-Milstein") +
    theme_minimal()
  print(p)
  return(df)
}

prueba <- SIMULAR_EULER(sigma, T, trayectorias, n)
