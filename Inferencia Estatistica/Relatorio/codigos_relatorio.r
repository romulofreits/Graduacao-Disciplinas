# Questa01

pa = pchisq(11.721, 15, lower.tail = TRUE);pa;round(pa, 3)
p5 = qchisq(0.50, 15);p5;round(p5, 3) # Mediana de X

# Dados populacionais:
r = 15 # g.l.
mu = r # Media
Mdn = p5 # Mediana
Mo = max(r-2, 0) # Moda

# Definindo a fun??o e plotando os gr?ficos:
f1 = function(x) dchisq(x, 15)
plot(f1, 0, 36, main = 'Distribui??o Qui-Quadrado com 15 g.l.', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=mu, col = 'blue') # Media
abline(v=Mo, col = 'red') # Moda
abline(v=Mdn, col = 'green4') # Mediana
#Legenda
legend("topright", legend = c('M?dia = 15', 'Moda = 13', 'Mediana = 14,339'), 
       col = c('blue', 'red', 'green4'), lwd = 5)

# Dados amostrais:
set.seed(521353)
amostra1 = rchisq(50, 15);sort(amostra1)
mean(amostra1);median(amostra1);var(amostra1);sd(amostra1)
summary(amostra1)
##################################################################
# Criando o QQ plot
qqplot(qchisq(ppoints(50), df = 15), amostra1, main = "Q-Q plot")
# Adicionando uma linha de refer?ncia
abline(0, 1, col = "red")
# Shapiro Teste
shapiro.test(amostra1)
boxplot(amostra1, col = 'tomato', main = 'Boxplot da Amostra', horizontal = TRUE)

# GR?FICO HACHURADO A
plot(x, f1,type="l", main="P(X < 11, 721)", ylab="f(x)")
abline(h=0, col='black')
abline(v=11.721, col = 'tomato')
ax=c(0,0,x[x<11.721],11.721,11.721)
ay=c(0,dchisq(c(0,x[x<11.721],11.721),15),0)
polygon(ax,ay, dens = 50, col = 'tomato')
#Legenda
legend("topright", legend = c('P(X < 11, 721) = 0,30'), 
       col = c('tomato'), lwd = 5)

# Histograma
hist(amostra1, prob = T, col = 'azure3',
     main = 'Histograma da Amostra n = 50', ylab = 'Densidade', ylim = c(0, 0.1))
curve(f1, add = T, col = 'red')
lines(density(amostra1), col = 'blue')
#Legenda
legend("topright", legend = c('Popula??o', 'Amostra'), 
       col = c('red', 'blue'), lwd = 5)
#________________________________________________________________#
# Questao 2

# X ~ t(21)
pa = pt(0.859, 21);pa;round(pa,3)
d6 = qt(0.60, 21);d6;round(d6, 3) # Decil 6 de X

# Dados populacionais:
n = 21 # g.l.
mu = 0 # Media
Mo = 0 # Moda
Mdn = qt(0.50, 21);Mdn;round(Mdn, 3) # Mediana

# Gerando a fun??o e plotando o gr?fico:
f2 = function(x) dt(x, 21)
plot(f2, -6, 6, main = 'Distribui??o t-Student com 21 g.l.', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=0, col = 'red') # Media, Moda e Mediana
#Legenda
legend("topright", legend = c('Mu = Mo = Md = 0'), 
       col = c('red'), lwd = 5)

# Dados amostrais:
set.seed(521353)
amostra2 = rt(50, 21);sort(amostra2)
mean(amostra2);median(amostra2);var(amostra2);sd(amostra2)
summary(amostra2)
shapiro.test(amostra2)
boxplot(amostra2, horizontal = TRUE,
        col = 'tomato', main = 'Boxplot da Amostra')

# Criando o QQ plot
qqplot(qt(ppoints(50), df = 21), amostra2, main = "Q-Q plot")
# Adicionando uma linha de refer?ncia
abline(0, 1, col = "red")
shapiro.test(amostra2)

# GRAFICO HACHURADO A
plot(f2, -6, 6, main = 'P(X < 0,859)', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=0.859, col = 'tomato')
polygon(x = c(-6, seq(-6, 0.859, l=50),0.859),
        y = c(0, f2(seq(-6, 0.859,l=50)), 0),
        col = 'tomato', density = 40, text())
#Legenda
legend("topright", legend = c('P(X < 0,859)=0,80'), 
       col = c('tomato'), lwd = 5)

# GRAFICO HACHURADO B
plot(f2, -6, 6, main = 'Decil 6 de X', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=0.257, col = 'tomato')
polygon(x = c(-6, seq(-6, 0.257, l=50),0.257),
        y = c(0, f2(seq(-6, 0.257,l=50)), 0),
        col = 'tomato', density = 40)
#Legenda
legend("topright", legend = c('D6 = 0,257'), 
       col = c('tomato'), lwd = 5)

# Histograma
hist(amostra2, prob = T, col = 'azure3', 
     main = 'Histograma da Amostra n = 50', ylab = 'Densidade', xlim = c(-4, 4))
curve(f2, add = T, col = 'red')
lines(density(amostra2), col = 'blue')
#Legenda
legend("topright", legend = c('Popula??o', 'Amostra'), 
       col = c('red', 'blue'), lwd = 5)

#___________________________________________________________________#
# Questao 3
# X ~ F(15,15)
pa = pf(2.40,15,15);pa;round(pa,2)
# Percentil 5 de X
p5 = qf(0.05, 15, 15, lower.tail = TRUE);p5;round(p5,2) 

# Dados populacionais:
m = 15
n = 15
Md = n/(n-2);Md;round(Md, 2) # Media
Mo = (n*(m-2))/(m*(n+2));Mo;round(Mo,2) # Moda
Mdn = qf(0.5, 15, 15, lower.tail = TRUE); Mdn;round(Mdn, 2) # Mediana

# Definindo a fun??o e plotando o gr?fico
f3 = function(x) df(x, 15, 15)
plot(f3, 0, 5, main = 'Distribui??o F-Snedecor com m = 15 g.l. e n = 15 g.l.', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=Md, col = 'blue') # Media
abline(v=Mo, col = 'red') # Moda
abline(v=Mdn, col = 'green4') # Mediana
#Legenda
legend("topright", legend = c('Média = 1,15 ', 'Moda = 0,76', 'Mediana = 1'), 
       col = c('blue', 'red', 'green4'), lwd = 3)

# Dados amostrais:
set.seed(521353)
amostra3 = rf(50, 15, 15);sort(amostra3)
mean(amostra3);median(amostra3);var(amostra3);sd(amostra3)
summary(amostra3)

# Criando o Q-Q plot
qqplot(qf(ppoints(50), df1 = 15, df2 = 15), amostra3, main = "Q-Q plot")
# Adicionando uma linha de refer?ncia
abline(0, 1, col = "red")

# Shapiro teste
shapiro.test(amostra3)

boxplot(amostra3, col = 'tomato', horizontal = TRUE, main = 'Boxplot da Amostra')


# GRAFICO HACHURADO A
plot(f3, 0, 5, main = 'P(X < 2,40)', ylab = 'f(x)')
abline(h=0, col='black')
polygon(x = c(0, seq(0, 2.40, l=50),2.40),
        y = c(0, f3(seq(0, 2.40,l=50)), 0),
        col = 'tomato', density = 40)
abline(v=2.40, col = 'tomato')
#Legenda
legend("topright", legend = c('P(X < 2,40) = 0,95'), 
       col = c('tomato'), lwd = 5)

# GRAFICO HACHURADO B
plot(f3, 0, 5, main = 'Quinto Percentil de X', ylab = 'f(x)')
abline(h=0, col='black')
abline(v=0.42, col = 'red')
polygon(x = c(0, seq(0, p5, l=50),p5),
        y = c(0, f3(seq(0, p5,l=50)), 0),
        col = 'tomato', density = 40)
#Legenda
legend("topright", legend = c('P5 = 0,42'), 
       col = c('tomato'), lwd = 5)

# Histograma
hist(amostra3, prob = T, col = 'azure3', 
     main = 'Histograma da Amostra n = 50', ylab = 'Densidade', ylim = c(0, 1))
curve(f3, add = T, col = 'red')
lines(density(amostra3), col = 'blue')
#Legenda
legend("topright", legend = c('Popula??o', 'Amostra'), 
       col = c('red', 'blue'), lwd = 5)

#____________________________________________________________#
#Questao 4
# X ~ N(40, 25)

# P(|X-40|<12)
mu=40;sigma=5
z=12/sigma;z
p=2*0.4918;p
pnorm(2.4)
pnorm(-2.4)
round(pnorm(2.4)-pnorm(-2.4),5)

# Percentil de ordem 5
round(qnorm(0.05,40,5),3)

f4 = function(x) dnorm(x, mean = 40, sd = 5)
plot(f4, 25,55, main = 'X~N(40,25)', ylab = 'f(x)')
abline(h=0, col = 'black')
abline(v=0.9836, col = 'red')
polygon(x = c(-6, seq(-6, 0.859, l=50),0.859),
        y = c(0, f4(seq(-6, 0.859,l=50)), 0),
        col = 'red', density = 40)

abline(v=40, col = 'red')
#Legenda
legend("topright", legend = c('mu=Mo=Md=40'), col = c('red'), lwd = 5)

# Dados amostrais:
set.seed(521353)
amostra4 = rnorm(50, 40, 5);sort(amostra4)
mean(amostra4);median(amostra4);var(amostra4);sd(amostra4)
summary(amostra4)
boxplot(amostra4, col = 'tomato', main = 'Boxplot da Amostra', horizontal = TRUE)
#################################################################################
# Criando o QQ plot
qqplot(qnorm(ppoints(50), mean = 40, sd = 5), amostra4, main = "Q-Q plot")
# Adicionando uma linha de referencia
abline(0, 1, col = "red")
shapiro.test(amostra4)

# Teste de Kolmogorov 0,Sminorv
ks.test(amostra4, 'pnorm', 40, 5)
# Com um p-valor de 5%, podemos supor que os dados da amostra seguem uma normal

# Histograma
hist(amostra4, prob = T, col = 'azure3', 
     main = 'Histograma da Amostra n = 50', ylab = 'Densidade', xlim = c(25, 55))
curve(f4, add = T, col = 'red')
lines(density(amostra4), col = 'blue')
#Legenda
legend("topright", legend = c('Popula??o', 'Amostra'), 
       col = c('red', 'blue'), lwd = 5)

# GRAFICO HACURADO BZ
p5 = qnorm(0.05);p5;round(p5,2)
plot(f4_1, -3,3, main = 'Z~N(0,1)', ylab = 'f(x)')
abline(h=0, col = "black")
abline(v=p5, col = "tomato")
polygon(x = c(-3, seq(-3, p5, l=50), p5),
        y = c(0, f4_1(seq(-3, p5,l=50)), 0),
        col = "tomato", density = 40)
#Legenda
legend("topright", legend = c('Percentil 5 = -1,64'), 
       col = c('tomato'), lwd = 5, cex = 1)


# GRAFICO HACURADO B
p_5 = round(qnorm(0.05,40,5),3); p_5
plot(f4, 25,55, main = 'X~N(40,25)', ylab = 'f(x)')
abline(h=0, col = "black")
abline(v=p_5, col = "tomato")
polygon(x = c(25, seq(25, p_5, l=50), p_5),
        y = c(0, f4(seq(25, p_5,l=50)), 0),
        col = "tomato", density = 40)
#Legenda
legend("topright", legend = c('Percentil 5 = 31,776'), 
       col = c('tomato'), lwd = 5, cex = 1)

# GRAFICO HACHURADO AZ
plot(f4_1, -3,3, main = "P( |Z| < 2.4 )", ylab = 'f(x)')
abline(h=0, col = "black")
abline(v=-2.4, col = "red")
abline(v=2.4, col = "red")

polygon(x = c(-2.4, seq(-2.4, 0, l=50), 0),
        y = c(0, f4_1(seq(-2.4, 0,l=50)), 0),
        col = "tomato", density = 40)

polygon(x = c(0, seq(0, 2.4, l=50), 2.4),
        y = c(0, f4_1(seq(0, 2.4,l=50)), 0),
        col = "tomato", density = 40)
#Legenda
legend("topright", legend = c("P(|Z|<2,4)=0,9836"), 
       col = c('tomato'), lwd = 5, cex = 1)

#GRAFICO HACHURADO A
f = function(x) dnorm(x,40,5)
plot(f, 25,55, ylab = 'f(x)') # O xaxt="n" remove os valores do eixo X
abline(h=0, col = 'black')
abline(v=28, col = 'tomato')
abline(v=52, col = 'tomato')
title( main = "P(|X-40|<12)")

ax = c(28,seq(28, 52, length.out = 30), 52 )
ay = c(0,lapply(seq(28, 52, length.out = 30),f),0)
polygon(ax,ay, dens=40,col='tomato')
#Legenda
legend("topright", legend = c("P(|X-40|<12)=0,9836"), 
       col = c('tomato'), lwd = 5, cex = 1)


