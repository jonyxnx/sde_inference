library(ggplot2)

SIMULAR_PROBAS <- function(sigma, T, trayectorias, n){
  forward_curve <- function(x) {
    return(0.15+ dgamma(x,3,2)/10) #usar gamma
  }
  
  # Simulación de trayectorias
  dt <- T / n
  r <- matrix(0, nrow = trayectorias, ncol = n + 1)
  r[,1] <- forward_curve(0)
  
  for (j in 1:trayectorias) {
    for (i in 1:n) {
      t <- i * dt
      s <- (i-1)*dt
      r[j, i + 1] <- rnorm(1, r[j,i]+forward_curve(t)-forward_curve(s)+(sigma^2)*((t^2-s^2)/2), sigma*sqrt(dt))
    }
  }
  
  # Gráfico de las trayectorias
  time <- seq(0, T, length.out = n + 1)
  df <- data.frame(time = rep(time, trayectorias), r = c(t(r)))
  p <- ggplot(df, aes(x = time, y = r, group = factor(rep(1:trayectorias, each = n + 1)))) +
    geom_line(col = 'purple', alpha = 0.5) +
    labs(x = "Tiempo (años)", y = "Tasa de interés", title = "Simulación de Ho-Lee con Probabilidad de transición") +
    theme_minimal()
  print(p)
    return(df)
}


# Parámetros del modelo
sigma <- 0.1  # Volatilidad
T <- 10     # Tiempo al vencimiento (años)
trayectorias <- 1  # Número de trayectorias simuladas
n <- 1000  # Número de pasos de tiempo
SIMULAR_PROBAS(sigma, T, trayectorias, n)
