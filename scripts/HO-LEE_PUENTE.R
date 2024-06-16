T <- 10
N <- 10000

puente_difusion <- function(N,T, alpha, beta, sigma){
  #Realizamos el vector de tiempos
  t <- seq(0, T, length.out = N)
  dt <- T/N
  
  
  #Creamos las dos funciones de tasas forward 

  forward_curve <- function(x) {
    return(alpha  + dnorm(x,5,1))
  }
  
  forward_curve2 <- function(x) {
    return(beta  + dnorm(x,5,1))
  }
  
  
  #Creamos las dos funciones theta theta <- function(t) {
  theta <- function(t, sigma) {
    df_dt <- (forward_curve(t + dt) - forward_curve(t)) / dt
    return(df_dt + 0.05 * sigma^2 * t)
  }
  
  theta2 <- function(t, sigma) {
    df_dt <- (forward_curve2(t + dt) - forward_curve2(t)) / dt
    return(df_dt + 0.5 * sigma^2 * t)
  }
  

  #Creamos los procesos x1 y x2
  
  x1<-numeric(N)
  x1[1]<-forward_curve(0)
  x2<-numeric(N)
  x2[1]<-forward_curve2(0)
  
  
  
  for(i in 1:(N-1)){
    x1[i+1]<-x1[i] + theta(i * dt, sigma) * dt + sigma * sqrt(dt) * rnorm(1)
    x2[i+1]<-x2[i] + theta2(i * dt, sigma) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  
  #Creamos los procesos y1 y y2
  y1<-numeric(length(t))
  y2<-numeric(length(t))
  for (i in 1:(N)){
    y1[i]<-x1[i]
    y2[i]<-x2[N-i+1]
  }
  #Verificamos que haya una interseccion de los procesos
  if(y1[1]>=y2[1]){
    indice<-1
    bandera<-TRUE
    while(bandera && indice <= length(t)){
      if (y1[indice] < y2[indice]){
        bandera = FALSE
      }else{
        indice<-indice + 1
      }
      v<-indice
    }
    9
  }else if (y1[1] < y2[1]){
    indice<-1
    bandera<-TRUE
    while(bandera && indice <= length(t)){
      if (y1[indice] > y2[indice]){
        bandera = FALSE
      }else{
        indice<-indice + 1
      }
      v<-indice
    }
  }
  if (bandera){
    return(puente_difusion(N, T, alpha, beta, sigma))
  }
  y<-numeric(length(t))
  for (i in 1:v-1){
    y[i]<-y1[i]
  }
  for (i in v:length(t)){
    y[i]<-y2[i]
  }
  return(data.frame(t,y))
}

alpha <- .15
beta <- .1
sigma <- .1

set.seed(234)
P <- puente_difusion(1000,10, alpha, beta, sigma)

ggplot(P, aes(x = t, y = y)) +
  geom_line(color = "purple") +
  labs(title = "Puente de difusión", x = "Tiempo (años)", y = "Tasa de interés") +
  theme_minimal() 
