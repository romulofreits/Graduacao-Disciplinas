###CC0288-Infer�ncia Estat�stica I

###Prof. Maur�cio Mota

##Calcule Gama(4)=3!=6 usando uma fun��o.


fx=function(x) x^3*exp(-x)

I_1=integrate(fx,0,Inf)$value;I_1

###agora calcule usando uma fun��o pr�pria do R


gamma(4);factorial(3)


####Calcule agora Gama(1/2)

gamma(1/2);sqrt(pi)


###Esboce o gr�fico de f(x)  

plot(fx,0,15)
abline(h=0,col="red")



#####Simuleo lan�amento de uma moeda honesta 100 vezes ou simule uma amostra
##### aleat�ria de tamnho 100 de X~Ber(p=1/2)
#### 

set.seed(322023) #####Cada aluno usa seu n�mero de matr�cula como semente aleat�ria.

A=rbinom(100,1,1/2);A


table(A)

####%57 fracassos e 43 coroas
n=100

s=sum(A);s

p_est=s/n;p_est  ####Comentar



