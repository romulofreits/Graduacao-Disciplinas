n = 10
p=1/2
#p1 = P(Y>=7)=1-P(Y<=6)
p1e = 1- pbinom(6,n,p);p1e
round(p1e,3)

#Aproxima pela normal
mu = n*p;mu
sigma2 = n*p*(1-p);sigma2 
sigma = sqrt(sigma2);sigma
z = (7-0.5-mu)/sigma;z
z = 0.94
p1a = 1 - pnorm(z);p1a
round(p1a,4)

round(1 - pnorm(0.94),3)

#p2 = P(Y<=5)
p2 = pbinom(5,10, 0.5);p2

z2 = (5 + 0.5-mu)/sigma;z2 
p2a = pnorm(z2); p2a;p2

#Exemplo 11.15
n = 400 #Tamanho da amostra

#Sucesso preferir a marca A e seja P a probabilidade de sucesso
# S numero de sucesso da amostra, segue uma Bin(n,p)
# s é o valor obtido na amostra

s = 240
#Simule uma amostra de tamanho 400 de x Bernouli p=0.6
set.seed(32)
A = rbinom(400, 1, 0.6);A
table(A)

set.seed(32)
A = rbinom(400, 1, 0.6);A
table(A)

set.seed(33)
A = rbinom(400, 1, 0.6);A
table(A)

set.seed(514096)
A = rbinom(400, 1, 0.6);A
table(A)

#Voltando ao problema
#Calcular a estimativa pontual para P
p_est = s/n;p_est

#Estimativa intevalar com confiança de 95%
gama = 0.95
alfa = 1- gama;alfa
alfa/2
(1-alfa)/2
qnorm(1-alfa/2)
zt = 1.96
#Intervalo de confiança conservador
ec = zt/sqrt(4*n);ec

IC195 = p_est +c(-1,1)* ec;IC195  

#Intervalo de confiança mv
emv = zt*sqrt(p_est * (1-p_est)/n);emv

IC295 = p_est +c(-1,1)* emv;IC295


#Intervalo de confiança mv com coreçao finita
emvc = zt/(n*sqrt(n))* sqrt((s-0.5)*(n-s+0.5));emvc 

IC395 = (s-0.5)/n + c(-1,1) * emvc; IC395
#Fazendo direto no R, intervalos de confiança exatos

binom.test(s,n)$conf.int
binom.test(s,n,conf.level=0.90)$conf.int

#Fazendo direto no R, intervalos de confiança aproximados

prop.test(s,n)$conf.int
prop.test(s,n)

prop.test(s,n, correct=FALSE)
IC295


n
a =  1 + 1.96^2/n;a
b = -(2*0.6 +1.96^2/n);b
c = 0.36
polyroot(c(c,b,a))

