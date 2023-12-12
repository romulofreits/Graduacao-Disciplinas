# Estatistica Nao Parametrica 26/10

my_function <- function(k, grupo1, grupo2){
  
  dados <- c(grupo1, grupo2)
  n1 <- length(grupo1)
  n2 <- length(grupo2)
  n <- n1 + n2
  D_obs <- mean(grupo1) - mean(grupo2)
  
  vetor_D_k <- NULL
  for(i in 1:k){
    amostra_k <- sample(dados, replace = FALSE)  
    grupo1_k <- amostra_k[1:n1]
    grupo2_k <- amostra_k[(n1+1):n]
    D_k <- mean(grupo1_k) - mean(grupo2_k)
    vetor_D_k[i] <- D_k
  }
  
  pvalor_d <- mean((vetor_D_k >= D_obs)*1)
  par(mar = c(4.5,4.5,0.1,0.1))
  hist(vetor_D_k, main = "", xlab = "Diferenças")
  box()
  text(20,150, paste("p-valor = ", pvalor_d)) #ajustar aqui!
}


grupo1 <- c(37,49,55,57)
grupo2 <- c(23,31,16)

my_function(1000, grupo1 = grupo1, grupo2 = grupo2)
