x=seq(0,36,l=150)
f1=dchisq(x,15)
plot(x, f1,type="l", main="P(X < 11, 721)", ylab="f(x)")
abline(h=0, col='black')
abline(v=11.721, col = 'tomato')
ax=c(0,0,x[x<11.721],11.721,11.721)
ay=c(0,dchisq(c(0,x[x<11.721],11.721),15),0)
polygon(ax,ay, dens = 50, col = 'tomato')
#Legenda
legend("topright", legend = c('P(X < 11, 721) = 0,30'), 
       col = c('tomato'), lwd = 5)

# Mediana de X
plot(f1, 0, 36, main = 'Mediana de X', ylab = 'f(x)')
abline(h=0, col='black')
p5 = qchisq(0.50, 15);p5;round(p5, 3) # Mediana de X
abline(v=p5, col = 'tomato')
legend("topright", legend = c('Mediana = 14,339'), 
       col = c('tomato'), lwd = 5)

p5 = qchisq(0.50, 15);p5;round(p5, 3) # Mediana de X
plot(x, f1,type="l", main="Mediana de X", ylab="f(x)")
abline(h=0, col='black')
abline(v=14.339, col = 'tomato')
ax=c(0,0,x[x<14.339],14.339,14.339)
ay=c(0,dchisq(c(0,x[x<14.339],14.339),15),0)
polygon(ax,ay, dens = 50, col = 'tomato')
#Legenda
legend("topright", legend = c('Md = 14,339'), 
       col = c('tomato'), lwd = 5)
