###CC0288-Inferência Estatística I

###Prof. Maurício Mota

##Calcule Gama(4)=3!=6 usando uma função.


fx=function(x) x^3*exp(-x)

I_1=integrate(fx,0,Inf)$value;I_1

###agora calcule usando uma função própria do R


gamma(4);factorial(3)


####Calcule agora Gama(1/2)

gamma(1/2);sqrt(pi)


###Esboce o gráfico de f(x)  

plot(fx,0,15)
abline(h=0,col="red")



#####Simuleo lançamento de uma moeda honesta 100 vezes ou simule uma amostra
##### aleatória de tamnho 100 de X~Ber(p=1/2)
#### 

set.seed(322023) #####Cada aluno usa seu número de matrícula como semente aleatória.

A=rbinom(100,1,1/2);A


table(A)

####%57 fracassos e 43 coroas
n=100

s=sum(A);s

p_est=s/n;p_est  ####Comentar



