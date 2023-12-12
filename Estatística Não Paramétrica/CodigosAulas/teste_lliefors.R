# Teste de Lilliefors

dados <- c(13.9, 17.7, 17.9, 18.3,
           18.5, 18.9, 19.4, 19.8,
           20.2, 20.6, 21.1, 21.3,
           21.7, 21.9, 22.0, 22.2,
           22.7, 22.8, 23.2, 23.3,
           23.4, 23.8, 24.4, 24.9)

D <- function(dados){
  zi <- (dados - mean(dados))/sd(dados)
  pi <- pnorm(zi)
  n <- length(dados)
  D_max <- NULL
  D_menos <- NULL
  
  for(i in 1:n){
    D_max[i] <- i/n - pi[i]   
    D_menos[i] <- pi[i] - (i-1)/n
  }
  
  D <- max(max(D_max), max(D_menos))
  
  D
  
}

print(D)
