library(dplyr)
library(ggplot2)

data <- read.csv("datos_mexico.csv")
data <- data[,c("TIME_PERIOD", "OBS_VALUE")]
data <- tail(data,120)
data <-head(data, 108)
data <- na.omit(data)
str(data)


year_base <- 2013

convertir_fecha <- function(fecha) {
  year <- as.numeric(substr(fecha, 1, 4))
  month <- as.numeric(substr(fecha, 6, 7))
  months_since_base <- (year - year_base) * 12 + (month - 1)
  return((months_since_base) / 12)
}


data <- data %>% mutate(indice = sapply(TIME_PERIOD, convertir_fecha))
years <- data[, "indice"]
rates <- data[, "OBS_VALUE"]/100

data <- data.frame(Year = years, Rate = rates)

ggplot(data, aes(x = Year, y = Rate)) +
  geom_line(color = "blue", size = 1) +         
  geom_point(color = "red", size = 2) +         
  labs(title = "Tasas de Interés a Corto Plazo", 
       x = "Años",                             
       y = "Tasa") +                     
  theme_minimal() +                          
  theme(plot.title = element_text(hjust = 0.5)) 

estimarContinuoCompleto <- function(rates, years){
  differences <- diff(rates)   
  squared_differences <- differences^2

  sum_squared_differences <- sum(squared_differences)

  sigma <- sqrt(sum_squared_differences / unname(years[length(years)])) # Calculamos sigma con variacion total
  
  P <- exp(-years*rates) # Calculamos los precios para un bono cupon cero con valor nominal 1 a tasa continua
  f <- numeric(length(P) - 1)
  
  for (i in 1:(length(P) - 1)) {
    f[i] <- (log(P[i] / P[i + 1])) / (years[i + 1] - years[i]) # Calculamos la tasa forward
  }
  df <- diff(rates)
  theta <- df+(sigma^2)*(years[-c(1)]^2)/2 # COnstruimos theta con los valores de las derivadas de la tasa forward
  print(sigma)
  return(list(theta,sigma))
}

estimacion <- estimarContinuoCompleto(rates, years)
theta <- unname(estimacion[[1]])
sigma <- unname(estimacion[[2]])
plot(years[-c(1)],theta)
print(theta)

THETA <- function(t, indices, theta){
  indice_inferior <- findInterval(t, indices)
  
  if (indice_inferior == length(indices) || indice_inferior==0) {
    return(0)  
  }
  
  indice_superior <- indice_inferior + 1
  valor <- theta[indice_inferior] + (t - indices[indice_inferior]) * (theta[indice_superior] - theta[indice_inferior]) / (indices[indice_superior] - indices[indice_inferior])

  return(valor)
}



tiempos <- unname(years[-c(1)])
print(tiempos)
T <- tiempos[length(tiempos)]
trayectorias <- 10
n <- 108
dt <- T/n

# Simulación de trayectorias
r <- matrix(0, nrow = trayectorias, ncol = n + 1)
r[, 1] <- rates[1]

for (j in 1:trayectorias) {
  for (i in 1:n) {
    dr <- THETA(i * dt, tiempos, theta)[1] * dt + sigma * sqrt(dt) * rnorm(1)
    r[j, i + 1] <- r[j, i] + dr
  }
}


# Gráfico de las trayectorias
time <- seq(0, T, length.out = n + 1)
df <- data.frame(time = rep(time, trayectorias), r = c(t(r)))
ggplot(df, aes(x = time, y = r, group = factor(rep(1:trayectorias, each = n + 1)))) +
  geom_line(color = 'blue', alpha = 0.5) +
  labs(x = "Tiempo (años)", y = "Tasa de interés", title = "Simulación modelo ajustado") +
  theme_minimal()

