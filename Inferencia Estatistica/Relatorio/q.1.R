
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
