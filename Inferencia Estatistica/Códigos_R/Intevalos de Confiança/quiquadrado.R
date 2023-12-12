#..............................................................#
#              Aula: Distribuição Qui Quadrado -11/04/22                #
#                                                              #
#             Prof. Gualberto e Prof. Mauricio                 #
#..............................................................#


# f(x; k) = [1/(2^{k/2}*Gama{k/2})]*x^{k/2-1}*exp{ -x/2 }, x > 0

#     E(X) = k  e    Var(X) = 2*k 

# dchisq(x, df, ncp = 0, log = FALSE)
# pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# rchisq(n, df, ncp = 0)


# Gráfico da densidade

curve(dchisq(x, df = 1), 0, 8, ylab = "f(x)", lty=1)
curve(dchisq(x, df = 2), 0, 8, add=T, col=2, lty=2)
curve(dchisq(x, df = 3), 0, 8, add=T, col=3, lty=3)
curve(dchisq(x, df = 4), 0, 8, add=T, col=4, lty=4)

legend(6, 1.3, c("k = 1", "k = 2", "k = 3", " k = 4"), 
       col=1:4, lty=1:4)
       
       

curve(dchisq(x, df = 10), 0, 70, ylab = "f(x)", lty=1)
curve(dchisq(x, df = 20), 0, 70, add=T, col=2, lty=2)
curve(dchisq(x, df = 30), 0, 70, add=T, col=3, lty=3)
curve(dchisq(x, df = 40), 0, 70, add=T, col=4, lty=4)

legend(55, 0.10, c("k = 10", "k = 20", "k = 30", " k = 40"), 
       col=1:4, lty=1:4)


# Vamos calcular probabilidades:

# P(X > 2,558) quando k = 10

z <- seq(0, 35, 0.1)
plot(z, dchisq(z, df =10), type="l", ylab="f(x)")
polygon(c(2.558,seq(2.558,35,0.1),35),
        c(0,dchisq(seq(2.558,35,0.1), df =10),0), 
        density=10, col = 'blue')

1 - pchisq(2.558, df = 10)
pchisq(2.558, df = 10, lower.tail = FALSE)


# P(X > 40,256) quando k = 30

z <- seq(0, 70, 0.1)
plot(z, dchisq(z, df =30), type="l", ylab="f(x)")
polygon(c(40.256,seq(40.256,70,0.1),70),
        c(0,dchisq(seq(40.256,70,0.1), df =30),0), 
        density=10, col = 'blue')

1 - pchisq(40.256, df = 30)
pchisq(40.256, df = 30, lower.tail = FALSE)


# P(X < a ) = 0.7 quando k = 20

z <- seq(0, 50, 0.1)
plot(z, dchisq(z, df =20), type="l", ylab="f(x)")
polygon(c(0,seq(0,qchisq(0.7, 20),0.1),qchisq(0.7, 20)),
        c(0,dchisq(seq(0,qchisq(0.7, 20),0.1), df =20),0), 
        density=10, col = 'blue')

qchisq(0.7, 20)
qchisq(0.3, 20, lower.tail = FALSE)

# P(X > a ) = 0.95 quando k = 20

z <- seq(0, 50, 0.1)
plot(z, dchisq(z, df =20), type="l", ylab="f(x)")
polygon(c(qchisq(0.05, 20),seq(qchisq(0.05, 20),50,0.1), 50),
        c(0,dchisq(seq(qchisq(0.05, 20),50,0.1), df=20),0), 
        density=10, col = 'blue')

qchisq(0.05, 20)
qchisq(0.95, 20, lower.tail = FALSE)

# P(X < 6.5 ) quando k = 2

pchisq(6.5, 2)

# Fazer outros mais na aula com valores que não estão na tabela










# Se Z ~ N(0,1), então Z^2 ~ X^2(1)

z <- rnorm(1000)
hist(z^2, prob=T)
curve(dchisq(x,df = 1), 0, 10, add=T, col='red')

# Se Z, Y ~ N(0,1) ind, então Z^2 + Y^2 ~ X^2(2)

z <- rnorm(1000); y <- rnorm(1000)
hist(z^2 + y^2, prob=T)
curve(dchisq(x,df = 2), 0, 15, add=T, col='red')


