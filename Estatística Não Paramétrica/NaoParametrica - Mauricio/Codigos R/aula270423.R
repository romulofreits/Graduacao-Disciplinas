###CC0291EStatística Não Paramétrica-27/04/23

##Victor Navarro


####Primeira Parte


# Considere o intervalo (Y1, Yn)
# gamma = 1 - p^n -(1-p)^n
# Calcule o valor mínimo de n para
# que o nível de confiança seja pelo
# menos 0,9 para os seguintes casos:
# a) P(Y1 <= q(0,25) <= Yn) 
# b) P(Y1 <= q(0,70) <= Yn) 
#g = function(n, p) 1 - p^n - (1-p)^n
ga = function(n) 1 - 0.25^n - 0.75^n - 0.9
gb = function(n) 1 - 0.7^n - 0.3^n 
plot(ga, 1, 12)
abline(h=0, col="red")
abline(v=8, col="red")
plot(gb, 1, 12)
abline(h=0.9, col="red")
abline(v=7, col="red")
n = 2:12
tabb = cbind(n,gb(n))

# Exemplo 3 pg 517 do Mood
# para uma a.a de tamanho 5, use (Y1, Y5) 
# como intervalo de tolerancia de 75% 
# da população
# Calcule o correspondente nível de probabilidade
# gama
bet = 0.75
j = 1
k = 5
j < k
n = 5
# Calculando os parâmetros da Beta
a = k - j
b = n - k + j + 1
a;b
gama = 1 - pbeta(bet, a,b); round(gama, 4)
gama1 = pbinom(k-j-1, n, bet); round(gama1, 4)







#####Segunda Parte -Teste do qui quadrado





####Exemplo 1.  Em seus experimentos com ervilhas, Mendel observou

####315 lisas e amarelas (tipo 1); 108 lisas e verdes (tipo 2);
### 101 estriadas e amarelas(tipo 3); 32 estriadas e verdes (tipo 4). 

###De acordo com a teoria da hereditariedade, os números
####deveriam estar na proporção: 9:3:3:1. Há evidência para duvidar dessa teoria
#### ao nível de significância de 0,01? Calcule o nível descritivo.

##X_i= número de ervilhas do tipo iobservadas nos n cruzmentos

##p_i=probabilidade da ervilha ser do tipo i, i=1,2,3,4.

##9+3+3=1=16

p=c(9,3,3,1)/16;p
sum(p)
###Freq. observadas

ervilha=c(315,108,101,32)

O=ervilha
n=sum(O);n


###Cáculo das freq. esperadas

E=n*p;E

?

mod=chisq.test(ervilha,p= p);mod  
names(mod)
mod$observed

mod$expected
mod$residuals

sum((mod$residuals)^2)

mod$stdres


Fazer na mão:

Qcal = sum(((O-E)^2)/E); Qcal

alfa = 0.01
k=4;gl=k-1;gl

Qtab = qchisq(1-alfa, gl); Qtab

# nd = P(Q >= Qcal)
nd = pchisq(Qcal, gl, lower.tail=FALSE); nd
nd < alfa
# Não rejeitamos H0

Qtab < Qcal
# Novamente, não rejeitamos H0




##   Questão do Mood Equilíbrio de Hardy-Weimberg


##P(A)=p; P(a)=1-p, P(AA)=P^2=P_1;P(Aa)+P(aA)=2p(1-p)=P_2;P(aa)=(1-p)^2=p_3

##p_1+p_2+p_3=1 



n1 = 10 ###Tipo 1

n2 = 53 ###Tipo 2

n3 = 46 ###Tipo 3

n = n1 + n2 + n3 ##Total de cruzamentos


# Estimativa de MV de p

# pest = (2n1+n2)/2n

pest = (2*n1+n2)/(2*n); pest

# Frequência esperada da primeira classefe1 = n*pest^2; fe1

fe2 = n*2*pest*(1-pest); fe2

fe3 = n*(1-pest)^2; fe3

fe1 + fe2 + fe3

p1est = pest^2; p1est

p2est = 2*pest*(1-pest); p2est

p3est = (1-pest)^2; p3est

p_est = c(p1est, p2est, p3est); p_est

sum(p_est)

esp = n*p_est; esp

obs = c(n1, n2, n3); obs

# Temos k = 3, m=1, gl = k - 1 - m

k = 3

m = 1

gl = k - 1 - m; gl

Qcal = sum(((obs-esp)^2)/esp); Qcal

alfa = 0.05

Qtab = qchisq(1-alfa, gl); Qtab

# nd = P(Q >= Qcal)

nd = pchisq(Qcal, gl, lower.tail=FALSE); nd

nd < alfa

# Não rejeitamos H0

Qtab < Qcal

# Novamente, não rejeitamos H0

# Diretamente no R:


chisq.test(obs,p=p_est)


####Os graus de liberdade estão errados ###

nd_certo=1-pchisq(0.91347,1);nd_certo;nd



