#X ~Beta(3,5). P(X<=0.3) = p
p = pbeta(0.3,3,5); p 

a = 3; b = 5; r = a+b-1; r
p = 0.3

#Y ~Bin(a+b-1,p) = Bin(7,0.3)
1-pbinom(a-1,r,p)

#Na prova:
a = 3; b = 1; r = a+b-1

#Questão da mediana na prova:
a = 13; b = 13
pbeta(0.45,a,b)

#Fazendo pela binomial
r = a+b-1; p = 0.45
1-pbinom(a-1,r,p)

n = 25
i = 0:n
pi = dbinom(i,r,p); pi
Pi = pbinom(i,r,p)
Si = 1+pi-Pi

tab = cbind(i,pi,Pi,Si); round(tab,4)

#S
set.seed(32)
t = runif(25,0,60)
t = round(t,2); t
sort(t)

#[Y14;Y23]
i = 14; j = 23
i<j

#terceiro quartil -> p = 0.75
p = 0.75
gama = pbinom(j-1,n,p) - pbinom(i-1,n,p); round(gama,2)

#Testar se d6 = 13:30 
d6 = 45/60; d6

#H0: d6 = 0.75 vs H1: d6 != 0.75
#Estatística de teste
#K = número de observações > 0.75 na amostra
#p = probabilidade de X > d6 = 0.4
#Teste equivalente a H0: p = 0.4 vs H1: p != 0.4
p =0.4
kobs= length(t[which(t>45)]); kobs
binom.test(kobs,25,p = 0.4)

aux1 = pbinom(kobs,25,p); aux1
aux2 = 1-pbinom(kobs-1,25,p); aux2
nd = 2*min(aux1,aux2); nd

#6º decil da amostra
quantile(t,0.6,type=5)
?quantile
n = 25; i = 1:25; p = 0.6
pi = (i-0.5)/n; pi

t
y = sort(t)

y15 = y[15]
y16 = y[16]

f15 = (p-pi[15])/(pi[16]-pi[15]); f15

d6 = (1-f15)*y15 +f15*y16; d6

d6 = (y15+y16)/2; d6

plot.ecdf(t)
curve(punif(x,0,60), add = T, col = 2)
abline(h = 0.6, col = 'blue')

ks.test(t, "punif",0,60)

n = 25; i = 1:25
Fe = i/n; Fe
F0 = y/60; F0
D = abs(Fe-F0); D
Dmax = max(D); Dmax

Femenos = c(0,Fe[1:24]); Femenos
Dmenos = abs(Femenos-F0); Dmenos
Dmenos_max = max(Dmenos); Dmenos_max

max(Dmax,Dmenos_max)

#Primeira questão da prova
x = c(2.8,4.7,5.4,6.3,7.8)
n = length(x); i = 1:n
Fe = i/n; Fe

plot.ecdf(x)
plot(c(0,x),c(0,Fe),type ="s", ylim = c(0,1))
