#Aula dia 30/03
# Q3P1

X = c(13,18,11,15,19,12,22)
Y = c(16,26,20,14,25,17,29) 

#Análise Exploratória
boxplot(X,Y, col = c('cyan','grey'),
        main = 'Boxplot de X e Y')

qqnorm(X)
qqline(X, col='red')

qqnorm(Y)
qqline(Y, col='red')

#Testar a Hipótese H0: rho = 0 vs H1: rho != 0
cor(X,Y)
cor.test(X,Y)

n = 7
r = 0.8094589
tcal = r*sqrt((n-2)/(1-r^2)); tcal
#nd = P(|t(5)|> 3.0825 )
p1 = pt(tcal,n-2); p1
nd = 2*min(p1,1-p1); round(nd,5)

#Análise Não Paramétrica (Populações Dependentes)
#H0: md(y)-md(x) = 0 vs. H1: md(y)-md(x) > 0
wilcox.test(Y,X,paired=T,alternative = "greater")
wilcox.test(X,Y,paired=T,alternative = "less")

#Fazer a distribuição de V no R
vmax = n*(n+1)/2; vmax
v = 0:vmax; v
pv = dsignrank(v,n); pv
Pv = psignrank(v,n)
S1v = 1-psignrank(v-1,n)
tab = cbind(v,pv,Pv,S1v); round(tab,4)

#Como obter V=27?
D = Y-X; D
mD = abs(D); mD
mDo = sort(mD); mDo
rD = rank(mDo); rD
Do = sort(D); Do

aux = sign(Do); aux
aux1 = aux*rD; aux1
v = sum(aux1[2:7]); v

#nível descritivo aproximado (nda)
n = 7
Ev = n*(n+1)/4; Ev
Varv = n*(n+1)*(2*n+1)/24; Varv
sigmav = sqrt(Varv); sigmav
#nda = P(V > 26.5) = P(Z > Z0)
z0 = (26.5-Ev)/sigmav; z0

nda = 1-pnorm(z0); nda 

#nd exato (nde)
nde = 1-psignrank(26,n); nde

nda;nde

#Análise Não Paramétrica (Populações Independentes)
n = 7; m = 7; N = m+n
wilcox.test(Y,X,alternative = 'greater')

#nd exato (nde) = P(Us >= 38) = P(Ws >= 66)
nde = 1-pwilcox(37,m,n); round(nde,5)

w = 38 + m*(m+1)/2; w

#Qual a distribuição de U neste caso?
Umax = m*n; Umax
u = 0:Umax; u
pu = dwilcox(u,m,n); pu

EU = sum(u*pu); EU; m*n/2
VU = sum((u-EU)^2*pu); VU; m*n*(N+1)/12

require(MASS)
fractions(EU)
fractions(VU)

#Distribuição nula de W
w = u+m*(m+1)/2; w
pw = pu; pw

EW = sum(w*pw); EW; m*(N+1)/2; fractions(EW)
VW = VU; VW

#nível descritivo aproximado (nda) 
z = (37.5-EU)/sqrt(VU); z 
nda = 1-pnorm(z); round(nda,5)

#Análise Paramétrica (Supondo Normalidade)
alfa = 0.05
t.test(Y,X,paired=T,alternative = "greater") 

n = 7
mD = mean(D); mD
sD = sd(D); sD

tcal = sqrt(n)*mD/sD; tcal

#nd = P(t(6)>= tcal)
nd = pt(tcal,n-1,lower.tail = F); round(nd,6)

#nd > alfa?
nd > alfa #Conclusão: Rejeitar H0, m(y)>m(x)

ttab = qt(1-alfa,n-1); ttab

#ttab < tcal?
ttab < tcal #Conclusão: Rejeitar H0, m(y)>m(x)

#Limite Inferior LI do IC para a média
LI = mD - ttab*sD/sqrt(n); LI

#nda: nível descritivo aproximado
