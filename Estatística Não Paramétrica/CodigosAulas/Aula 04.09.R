## Estatistica Nao Parametrica 

# Densidade da Poisson:
dpois(0, 1.2)
dpois(1, 1.2)
dpois(2, 1.2)
dpois(3, 1.2)
dpois(4, 1.2)
dpois(5, 1.2)

#------------------------------------------------
f0 = c(15, 25, 10, 5, 4, 1) #frequencia observada
x = rep(1:5, f0)
ks.test(f0, 'ppois', 1.2)

# grafico da acumulada:
ecdf(x)
plot(ecdf(x))

ppois(0, 1.2)
ppois(1, 1.2)
ppois(2, 1.2)
ppois(3, 1.2)
ppois(4, 1.2)
ppois(5, 1.2)

a = round(ppois(0:5, 1.2), 2) # podemos criar uma seq com a acumulada
b = round(dpois(0:5, 1.2), 2)
c = dpois(0:5, 1.2)

# calculando a acumulada empirica:
inicio = c(15/60)
for (i in 1:5){
  inicio[i+1] = inicio[i] + (f0[i+1]/60)
}

#---------------------------------------------------------------




