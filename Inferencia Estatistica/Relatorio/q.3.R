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

ks.test(amostra3, 'pnorm', mean(amostra3), sd(amostra3))
# Há indícios que, ao nível de 5%, os seguem 

boxplot(amostra3, col = 'tomato', horizontal = TRUE, main = 'Boxplot da Amostra')

shapiro.test(amostra3)


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
