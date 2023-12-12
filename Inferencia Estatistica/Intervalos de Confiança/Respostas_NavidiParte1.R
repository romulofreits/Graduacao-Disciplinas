# Lista Inferência Navidi Parte 1

#Q.09
n = 74 #tamanho da amostra
Xb = 3.8 #media amostral
Sd = 4.8 #desvi padrao amostral

#a) fazer um intervalo de confiança de 95% para a média populacional

gama = 0.95 #nivel de confianca
alfa = 1 - gama; alfa #nivel de significancia
alfa/2
qt1 = qnorm(1-alfa/2) #definindo o quantil
round(qt1, 3)
qt1 = 1.96

e1 = qt1 * Sd/sqrt(n); e1 #definindo o valor do erro
IC95 = Xb + c(-1,1) * e1;IC95 #calculando o intervalo de confiança
round(IC95, 4)

#b) fazer um intervalo de confiança de 98% para a media populacional

gama = 0.98
alfa = 1 - gama;alfa
alfa/2
qt2 = qnorm(1-alfa/2)
round(qt2, 3)
e2 = qt2 * Sd/sqrt(n);e2
IC98 = Xb + c(-1,1) * e2;IC98
round(IC98, 4)

#d) gama = 0.95 ; erro = 0.7
gama = 0.95 #nivel de confianca
alfa = 1 - gama; alfa #nivel de significancia
alfa/2
qt1 = qnorm(1-alfa/2);qt1 #definindo o quantil
round(qt1, 3)
erro = 0.7
N = ((qt1*Sd)/erro)^2; N

# Nesse caso, nossa amostra deveria ser de 181 locais

#*********************************************************
# Q. 10
# X = Capacidade (em ampere-hora)
n = 120
Xb = 178
Sd = 14

#a) intervalo de 95% de confiança
gama = 0.95
alfa = 1 - gama; alfa
alfa/2
z = qnorm(1-alfa/2);z
erro = z*Sd/sqrt(n);erro

IC95 = Xb + c(-1,1) * erro; IC95
# IC[mu;95%] = [175.4951 ; 180.5049]

#b) intervalo de 98% de confiança
gama = 0.98
alfa = 1 - gama;alfa
alfa/2
z = qnorm(1-alfa/2);z
erro = z*Sd/sqrt(n);erro
IC98 = Xb + c(-1,1) * erro; IC98
# IC[mu;98%] = [175.0269 ; 180.9731]

#d) tamanho da amostra para uma gama de 95% e um erro de 0.2

gama = 0.95 #nivel de confianca
alfa = 1 - gama; alfa #nivel de significancia
alfa/2
qt1 = qnorm(1-alfa/2);qt1 #definindo o quantil
erro = 0.2
N = ((qt1*Sd)/erro)^2; N
# 18824 Baterias

#e) tamanho da amostra para uma gama de 99% e um erro de 0.2

gama = 0.99 #nivel de confianca
alfa = 1 - gama; alfa #nivel de significancia
alfa/2
qt1 = qnorm(1-alfa/2);qt1 #definindo o quantil
erro = 0.2
N = ((qt1*Sd)/erro)^2; N
# 32511 Baterias

#**************************************************************






