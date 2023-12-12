#..............................................................#
#              Aula: Distribuição t de Student -08/05/23                #
#                                                              #
#             ProfsRonald, Gualberto e Prof. Mauricio                 #
#..............................................................#


# Seja Z uma v.a N(0,1) e Y uma v.a X^2(k), com Z e Y 
# independentes, então, t = sqrt(k)*Z/sqrt(Y) ~ t(k).


# f(x; k) = [Gama{(k+1)/2}/(Gama{k/2}sqrt{pi*k})]*[1+x^2/k]^{-(v+1)/2}, 
#           para -Inf < x < Inf                                  

#     E(X) = 0, k >1  e    Var(X) = k/(k-2), k > 2 

# dt(x, df, ncp = 0, log = FALSE)
# dt(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# dt(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# dt(n, df, ncp = 0)


# Gráfico da densidade

curve(function(x) dt(x, df = 4), -8, 8, ylab = "f(x)", lty=1)
curve(dt(x, df = 3), -8, 8, add=T, col=2, lty=2)
curve(dt(x, df = 2), -8, 8, add=T, col=3, lty=3)
curve(dt(x, df = 1), -8, 8, add=T, col=4, lty=4)

legend(5, 0.38, c("k = 4", "k = 3", "k = 2", " k = 1"), 
       col=1:4, lty=1:4)
       
       

curve(dt(x, df = 40), -5, 5, ylab = "f(x)", lty=1)
curve(dt(x, df = 30), -5, 5, add=T, col=2, lty=2)
curve(dt(x, df = 20), -5, 5, add=T, col=3, lty=3)
curve(dt(x, df = 10), -5, 5, add=T, col=4, lty=4)

legend(-5, 0.40, c("k = 40", "k = 30", "k = 20", " k = 10"), 
       col=1:4, lty=1:4)


# Vamos calcular probabilidades:

# P(-1.943 < X < 1.943) quando k = 6

z <- seq(-5, 5, 0.1)
plot(z, dt(z, df =6), type="l", ylab="f(x)")
polygon(c(-1.943,seq(-1.943,1.943,0.1),1.943),
        c(0,dt(seq(-1.943,1.943,0.1), df =6),0), 
        density=10, col = 'blue')

pt(1.943, df=6) - pt(-1.943, df=6) 


# P(X > 1.753) quando k = 15

z <- seq(-4, 4, 0.1)
plot(z, dt(z, df =15), type="l", ylab="f(x)")
polygon(c(1.753,seq(1.753,4,0.1),4),
        c(0,dt(seq(1.753,4,0.1), df =15),0), 
        density=10, col = 'blue')

1 - pt(1.753, df = 15)
pt(1.753, df = 15, lower.tail = FALSE)


# P(X < a ) = 0.7 quando k = 20

z <- seq(-4, 4, 0.1)
plot(z, dt(z, df =20), type="l", ylab="f(x)")
polygon(c(-4,seq(-4,qt(0.7,20),0.1),qt(0.7,20)),
        c(0,dt(seq(-4,qt(0.7,20),0.1), df =20),0), 
        density=10, col = 'blue')

qt(0.7, 20)
qt(0.3, 20, lower.tail = FALSE)

# P(X > a ) = 0.95 quando k = 20

z <- seq(-4, 4, 0.1)
plot(z, dt(z, df =20), type="l", ylab="f(x)")
polygon(c(qt(0.05,20),seq(qt(0.05,20),4,0.1),4),
        c(0,dt(seq(qt(0.05,20),4,0.1), df =20),0), 
        density=10, col = 'blue')

qt(0.05, 20)
qt(0.95, 20, lower.tail = FALSE)

# P(X < 4.5 ) quando k = 2

pt(4.5, 2)

# Fazer outros mais na aula com valores que não estão na tabela






# Se X ~ t(k), então Y = (1 + X^2/k)^{-1} ~ Beta(k/2, 1/2)

k <- 5
x <- rt(1000, k)
y <- (1 + x^2/k)^(-1)
hist(y, prob=T)
curve(dbeta(x,k/2,1/2), 0, 1, add=T, col='red')


# Se X ~ t(k), então Y = X^2 ~ F(1, k)

k <- 15
x <- rt(1000, k)
y <- x^2
hist(y, prob=T)
curve(df(x,1,k), 0, 20, add=T, col='red')


