#2)

#a)
x1 = seq(0,10,length = 30); x1

x2 = seq(0,10,length = 30); x2

zf = function(x1,x2) {
 y = 50 + 10*x1 + 7*x2  
}

z = outer(x1,x2,zf)

persp(x1,x2,z,xlab = "Velocidade de rotação",
      ylab = "temperatura",zlab = "Impulsão",col = "steelblue",
      expand = 0.5, ticktype = "detailed", theta = -45)

contour(x1,x2,z,xlab = "Velocidade de rotação", ylab="Temperatura")

###################################################

#b)
x1b = seq(0,10,length = 30); x1

x2b = seq(0,10,length = 30); x2

zfb = function(x1,x2) {
  y = 50 + 10*x1 + 7*x2 + 5*x1*x2  
}

zb = outer(x1b,x2b,zfb)

persp(x1b,x2b,zb,xlab = "Velocidade de rotação",
      ylab = "temperatura",zlab = "Impulsão",col = "gray",
      expand = 0.5, ticktype = "detailed", theta = 50)

contour(x1,x2,z,xlab = "Velocidade de rotação", ylab="Temperatura")

##################################################

#c)
x1c = seq(0,10,length = 30); x1c

x2c = seq(0,10,length = 30); x2c

zfc = function(x1,x2) {
  y = 800 + 10*x1 + 7*x2 -8.5*x1^2 - 5*x2^2 + 4*x1*x2  
}

zc = outer(x1c,x2c,zfc)

persp(x1c,x2c,zc,xlab = "Velocidade de rotação",
      ylab = "temperatura",zlab = "Impulsão",col = "steelblue",
      expand = 0.5, ticktype = "detailed", theta = 50)

contour(x1c,x2c,zc,xlab = "Velocidade de rotação", ylab="Temperatura")

###################################################

# 3)
library(mvtnorm)
mu = c(0,0); mu
sigma = matrix(c(1,0,0,1), ncol = 2); sigma
x1 = seq(-10,10, length = 50)
x2 = x1

f = matrix(0,length(x1),length(x2))

for(i in 1:length(x1))
  for(j in 1:length(x2))
    f[i,j] = dmvnorm(c( x1[i],x2[2] ),mu,sigma)


persp(x1, x2, f, col = "gray", theta = -35, expand = 0.5)

















