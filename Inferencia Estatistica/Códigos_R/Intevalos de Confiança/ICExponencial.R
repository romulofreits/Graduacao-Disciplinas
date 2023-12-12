###CC0288-INFERENCIA ESTATÍSTICA I- 08/05/2023


####Construido pelo Prof. Paulo Justiniano.



####Seja X~exp(teta), X_2,X_2,...,X_n ~aa de X

###S=X_1+X_2+...+X_n- Gama(n,teta)

#####V=2*teta*S ~qui^2(2*n)


####Vamos brincar com o R para analisar este caso.


#####Gere uma amostra de tamanho n=50 de   S~Gama(r=3,teta=2)

###A  lei de de V=2*teta*S= 4S -quiquadrado(6).


##E(V)=6;V(V)=12



###Vejam com EV e VarV se comportam à medida que o tamanho da amostra aumenta!!! a 

####Vamos fazer n=10!!!
set.seed(202332) 
S=rgamma(10,3,2)
V=4*S;mean(V);var(V)

#########################################################################



####Vamos fazer n=20!!!
set.seed(202332) 
S=rgamma(20,3,2)
V=4*S;mean(V);var(V)

#################################################################################




####Vamos fazer n=50!!!
set.seed(202332) 
S=rgamma(50,3,2)
V=4*S;mean(V);var(V)

#################################################################################


 


####Vamos fazer n=100!!!
set.seed(202332) 
S=rgamma(100,3,2)
V=4*S;mean(V);var(V)

#################################################################################








####Vamos fazer n=200!!!
set.seed(202332) 
S=rgamma(200,3,2)
V=4*S;mean(V);var(V)

#################################################################################









####Vamos fazer n=500!!!
set.seed(202332) 
S=rgamma(500,3,2)
V=4*S;mean(V);var(V)




####Vamos fazer n=1000!!!
set.seed(202332) 
S=rgamma(1000,3,2)
V=4*S;mean(V);var(V)



####Vamos fazer n=5000!!!
set.seed(202332) 
S=rgamma(5000,3,2)
V=4*S;mean(V);var(V)



####Vamos fazer n=10000!!!
set.seed(202332) 
S=rgamma(10000,3,2)
V=4*S;mean(V);var(V)


####Vamos fazer n=100000!!!
set.seed(202332) 
S=rgamma(100000,3,2)
V=4*S;mean(V);var(V)



####Vamos fazer n=1.000.000!!!
set.seed(202332) 
S=rgamma(1000000,3,2)
V=4*S;mean(V);var(V)  

####Vamos comentar######


####Computador não prova nada... Melhor destudar e demonstrar!!!!!














####intervalo de confiança para teta. 


####IC(teta,(1-alfa)*100\%)

### X~N(mu,sigma^2),sigma conhecido

##n=tamanho da amostra

##Na= número de amostras de tamanho n retiradas
 
cobertura.Normal=function(Na,n,alfa,mu,sigma){

conta=0

for(i in 1:Na){

xsim=rnorm(n,mu,sigma)

ic=mean(xsim)+qnorm(c(alfa/2,1 -alfa/2))*sigma *(1/sqrt(n))

if( mu >ic[1]& mu<ic[2]) conta=conta + 1
}
return(conta/Na)
}


###Confiança gama=1-alfa=0,95

cobertura.Normal(1000,2,0.05,10,1)

cobertura.Normal(1000,5,0.05,10,1)

cobertura.Normal(1000,10,0.05,10,1)
cobertura.Normal(1000,15,0.05,10,1)
cobertura.Normal(1000,20,0.05,10,1)
cobertura.Normal(1000,25,0.05,10,1)
cobertura.Normal(1000,30,0.05,10,1)
cobertura.Normal(1000,35,0.05,10,1)
cobertura.Normal(1000,40,0.05,10,1)
cobertura.Normal(1000,45,0.05,10,1)
cobertura.Normal(1000,50,0.05,10,1)
cobertura.Normal(1000,100,0.05,10,1)



####Comente!!!!!!
#############################################################


###Confiança gama=1-alfa=0,99


cobertura.Normal(1000,2,0.01,10,1)

cobertura.Normal(1000,5,0.01,10,1)

cobertura.Normal(1000,10,0.01,10,1)
cobertura.Normal(1000,15,0.01,10,1)
cobertura.Normal(1000,20,0.01,10,1)
cobertura.Normal(1000,25,0.01,10,1)
cobertura.Normal(1000,30,0.01,10,1)
cobertura.Normal(1000,35,0.01,10,1)
cobertura.Normal(1000,40,0.01,10,1)
cobertura.Normal(1000,45,0.01,10,1)
cobertura.Normal(1000,50,0.01,10,1)
cobertura.Normal(1000,100,0.01,10,1)

####Comente!!!!!!
#############################################################


##Confiança gama=1-alfa=0,90


cobertura.Normal(1000,2,0.10,10,1)

cobertura.Normal(1000,5,0.10,10,1)

cobertura.Normal(1000,10,0.10,10,1)
cobertura.Normal(1000,15,0.01,10,1)
cobertura.Normal(1000,20,0.10,10,1)
cobertura.Normal(1000,25,0.10,10,1)
cobertura.Normal(1000,30,0.10,10,1)
cobertura.Normal(1000,35,0.10,10,1)
cobertura.Normal(1000,40,0.10,10,1)
cobertura.Normal(1000,45,0.10,10,1)
cobertura.Normal(1000,50,0.10,10,1)
cobertura.Normal(1000,100,0.10,10,1)

####Comente!!!!!!
#############################################################


####Agora brinque com a função variando os parâmetros!!!!!!!!!!!!!






