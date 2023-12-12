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

        
