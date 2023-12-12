# X ~ t(21)
pa = pt(0.859, 21);pa;round(pa,3)
d6 = qt(0.60, 21);d6;round(d6, 3) # Decil 6 de X

# Dados populacionais:
n = 21 # g.l.
mu = 0 # Media
Mo = 0 # Moda
Mdn = qt(0.50, 21);Mdn;round(Mdn, 3) # Mediana

# Gerando a função e plotando o gráfico:
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
# Adicionando uma linha de referência
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
