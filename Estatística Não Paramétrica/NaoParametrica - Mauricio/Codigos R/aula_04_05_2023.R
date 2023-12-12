quantile
?quantile
#exercicio 19 lista especial 2
#a nossa vari�vel X = conte�do de titanio 

X = c(8.32, 8.05, 8.93, 8.85, 8.25, 8.46, 8.52,
      8.35, 8.36, 8.41, 8.42, 8.30, 8.71, 8.75,
      8.60, 8.83, 8.50, 8.38, 8.29, 8.47)
Y = sort(X); Y
rank(Y)  #####tirei o empate!!!!!!!!


#item a
ecdf(X) 
plot(ecdf(X))
n=length(X);n
i = 1:n;i
pi=(i-0.5)/n;pi
tab=cbind(i,pi);tab
#mediana de X usando a empirica e a alisada
#n = 20 � par

n/2;n/2+1
Y[10];Y[11]
mediana = (Y[10]+Y[11])/2;mediana
#mediana do R
median(X)
#mediana usando a alisada
p = 0.5
p >= pi[10]
p < pi[11]
f10 = (p-pi[10])/(pi[11]-pi[10]); f10
q2 = (1- f10)*Y[10]+f10*Y[11]; q2
#oitavo decil D8
p= 0.8 #oitavo decil D8
f16 = (p-pi[16])/(pi[17]-pi[16]); f16
D8 = (1- f16)*Y[16]+f16*Y[17]; D8
#no R
quantile(X, 0.8)

#qqplot
qqplot(X)
?qqplot
qqnorm(X)
qqline(X, col= 'red')

#teste de normalidade
shapiro.test(X)
lillie.test(X)
??lillie
require(nortest)
lillie.test(X, pnorm())
ks.test(X, "pnorm", mean(X), sd(X) )

x = c(8.32, 8.05, 8.93, 8.85, 8.25, 8.46, 8.52,
      8.35, 8.36, 8.41, 8.42, 8.30, 8.71, 8.75,
      8.60, 8.83, 8.50, 8.38, 8.29, 8.47)
ks.test(x, "pnorm", mean(x), sd(x) )
lillie.test(x)

#teste de sinais
#h0 é testar se a mediana é 8,5

X
#Se h0 é verdade

w=X-8.5;w

sort(w)

#como temos um 0
n1 = 19
# s = número de sinais positivos na amostra
s = 7

#nível descritivo
#h0: p = 1/2 vs h1: p diferente de 1/2
#direto no R
modelo1 = binom.test(s, n1); modelo1

#Obtenha na unha o nível descritivo
p1 = 1-pbinom(6, n1, 0.5); p1
p2 = pbinom(7, n1, 0.5); p2

nd = 2*min(p1,p2);nd
modelo1$p.value

wilcox.test(x, mu=8.5)

wilcox.test(X, w, paired = TRUE)
