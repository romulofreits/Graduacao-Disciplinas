# CC0291 - ESTATÍSTICA NÃO PARAMETRICA
# Marcel Caetano

# Tirei acentos para evitar erros de codificacao
# Graficos

x = c(8.32, 8.05, 8.93, 8.85, 8.25, 8.46, 8.52,
      8.35, 8.36, 8.41, 8.42, 8.30, 8.71, 8.75,
      8.60, 8.83, 8.50, 8.38, 8.29, 8.47)
y = sort(x); y

n = length(x)
i = 1:n
Fe = i/n # empirica
Fa = (i-0.5)/n # alisado

cbind(i,Fe,Fa)

old.par = par(mfrow = c(1,2)) #janela grafica: 1 linha e 2 colunas

# grafico da empirica   
# usando ecdf
ecdf(x)
plot(ecdf(x), main = "Dist. Empírica", ylab = "Fe(x)")
#points(c(sort(x)), Fa, col = "red") # Pontos da ecdf

# grafico da empirica (preta) com alisada (vermelha)
plot(ecdf(x), main = "Dist. Empírica e Alisada", ylab = "Fe(x)")
points(c(sort(x)), Fa, col = "red", pch = 21)
lines(sort(x), Fa, col = "red", lty = 1)

par(old.par) #fechar janela grafica

