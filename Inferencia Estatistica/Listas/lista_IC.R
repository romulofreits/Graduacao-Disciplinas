# Lista Intervalos de Confianca

# Q.4
Q4 = c(1.35, 1.40, 1.55, 1.80, 1.65, 2.00, 1.47, 1.81, 1.73, 
       2.03, 1.45, 1.95, 1.78, 1.87)

n = length(Q4);n #tamanho da amostra
Xb = mean(Q4);Xb #media
Sd = sd(Q4);Sd #desvio padrao

#a) gama = 95%
gama = 0.95
alfa = 1-gama;alfa
alfa/2
q95 = qnorm(1-alfa/2);q95
erro = q95 * Sd/sqrt(n); erro

IC95 = Xb + c(-1,1) * erro; IC95
round(IC95, 4)
# IC[mu;95%] = [1.5837 ; 1.8220]

#b)  valor do erro
erro = q95 * Sd/sqrt(n); erro
round(erro, 4)
# erro = 0.1191

#c) tamanho da amostra para um um erro de 0.005
erro = 0.005
N = ((q95*Sd)/erro)^2; N
# para um erro de 0.005, a amostra deveria ser de tamanho 7949
# A partir dessa nova amostra vamos estimar a variância:
n = 7949
gama = 0.95
alfa = 1-gama;alfa
