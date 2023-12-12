#Aula dia 23/03

#Exercício 13.10 do Bussab e Morettin

#*Sem empates

Con = c(6.0,6.6,6.8,6.9,7,7.1,7.2,7.3,7.4,8)
n = length(Con); n

Trat = c(6.61,6.7,6.81,6.82,6.83,6.84,6.85,6.91,6.92,7.5)
m = length(Trat); m

N = m+n; N

Sg = N*(N+1)/2; Sg #Soma geral dos postos

#*Análise Exploratória
boxplot(Con,Trat, col = c('red','blue'))

median(Con)
median(Trat)

# Teste direto
# H0: Delta = 0  vs H1: Delta != 0
wilcox.test(Trat,Con,conf.level = 0.95)

#Obtenção da estatística do teste
wmin = m*(m+1)/2; wmin
ws = 36+wmin; ws

#Invertendo os grupos
wilcox.test(Con,Trat)

Ws = 64+55; Ws

#ATENÇÃO: Teste que estima Delta
wilcox.test(Trat,Con, conf.int = TRUE)

#Distribuição exata de U
p1 = 1-pwilcox(35,m,n); p1
p2 = pwilcox(36,m,n); p2

nd = 2*min(p1,p2); nd
round(nd,3)

u = 0:(m*n); u
pu = dwilcox(u,m,n); pu
PU = pwilcox(u,m,n); PU

p2 = PU[37]; p2

tab = cbind(u,pu,PU);round(tab,2)
tail(round(tab,2),10)

#Exercício 13.11 item c. m = n = 3
m = 3; n = 3; N= m+n

#Número de arranjos possíveis
choose(N,m)

u = 0:(m*n); u 
pu = dwilcox(u,m,n); pu
PU = pwilcox(u,m,n); PU

tab = cbind(u,pu,PU); tab  

EU = sum(u*pu); EU; m*n/2

EU2 = sum(u^2*pu); EU2

VU = EU2 - EU^2; VU; m*n*(N+1)/12

#Distribuição exata de W
wmin = m*(m+1)/2; wmin
w = u+wmin; w
pw = pu; pw
fw = cbind(w,pw); fw

EW = sum(w*pw); EW; m*(N+1)/2

#Aproximar para a Normal

#*Com empates
Con = c(6.0,6.6,6.8,6.9,7,7,7,7.1,7.4,8)
n = length(Con); n

Trat = c(6.6,6.7,6.8,6.8,6.8,6.8,6.8,6.9,6.9,7.5)
m = length(Trat); m

N = m+n; N

CT = c(Con,Trat); CT #Todas os valores

table(CT)
d1 = 1; d2=2; d3=1; d4=6; d5=3; d6=3; d7=1; d8=1; d9=1; d10=1
d = c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10); d
Aux3 = sum(d^3-d); Aux3 #Fator de correção para empates
Aux1 = m*n*(N+1)/12; Aux1
Aux2 = m*n/(12*N*(N-1)); Aux2

VW = Aux1 - (Aux2*Aux3); VW #Variância com empates

CT_o = sort(CT); CT_o #Valores ordenados

CT_r = rank(CT); 

#Solução Paramétrica
#H0: muC = muT vs H1: muC != muT

summary(Con)
summary(Trat)

var(Con);var(Trat)
max(var(Con),var(Trat))/min(var(Con),var(Trat))

#Teste t
t.test(Trat,Con)

t.test(Trat,Con, var.equal = TRUE)

#Testes de Normalidade
shapiro.test(Con)
shapiro.test(Trat)

par(mfrow=c(1,2))
qqnorm(Con)
qqline(Con, col='red')
qqnorm(Trat)
qqline(Trat, col='red')
