#Voltagem de quebra
x = c(62,50,53,57,41,53,55,61,59,64,50,53,64,62,50,68,
      54,55,57,50,55,50,56,55,46,55,53,54,52,47,47,55,
      57,48,63,57,57,55,53,59,53,52,50,55,60,50,56,58);x
#Tamanho da amostra
n = length(x);n
Sx = sum(x); Sx
mean(x);Sx/n
boxplot(x,col="red")
y =  sort(x);y
summary(x)
var(x);sd(x)
hist(x,prob=TRUE)
f = function(x)dnorm(x,mean(x),sd(x))
plot(f,mean(x)-3*sd(x), mean(x)+3*sd(x))
hist(x,prob=TRUE)
curve(f, add=TRUE, col='red')
shapiro.test(x)
mod=t.test(x);mod
mod$conf.int
t.test(x,conf.level = 0.90)$conf.int
