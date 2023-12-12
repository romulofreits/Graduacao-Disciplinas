###CC0295-INFERENCIA ESTATÍSTICA II- 10/04/2017

####intervalo de confiança para mu. 


####IC(mu,(1-alfa)*100\%)

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

#############################################################

####intervalo de confiança para mu. 
### X~N(mu,sigma^2),sigma desconhecido
##n=tamanho da amostra
##Na= número de amostras de tamanho n retiradas
 
cobertura.Gosset=function(Na,n,alfa,mu,sigma){
conta=0

for(i in 1:Na){
xsim=rnorm(n,mu,sigma)

ic=mean(xsim)+c(qt(alfa/2,n-1),qt(1 -alfa/2,n-1))*sigma *(1/sqrt(n))

if( mu >ic[1]& mu<ic[2]) conta=conta + 1
}
return(conta/Na)
}
cobertura.Gosset(1000,2,0.05,10,1)
cobertura.Gosset(1000,5,0.05,10,1)
cobertura.Gosset(1000,10,0.05,10,1)
cobertura.Gosset(1000,15,0.05,10,1)
cobertura.Gosset(1000,20,0.05,10,1)
cobertura.Gosset(1000,25,0.05,10,1)
cobertura.Gosset(1000,30,0.05,10,1)
cobertura.Gosset(1000,35,0.05,10,1)
cobertura.Gosset(1000,40,0.05,10,1)
cobertura.Gosset(1000,45,0.05,10,1)
cobertura.Gosset(1000,50,0.05,10,1)
cobertura.Gosset(1000,100,0.05,10,1)


############################################################


####intervalo de confiança para sigma^2. 

IC(sigma^2,(1-alfa)*100%)


### X~N(mu,sigma^2), mu e sigma desconhecido
##n=tamanho da amostra
##Na= número de amostras de tamanho n retiradas


cobertura.Var=function(Na,n,alfa,mu,sigma){
conta=0

for(i in 1:Na){
xsim=rnorm(n,mu,sigma)
li=(n-1)*var(xsim)/qchisq(1-alfa/2,n-1)
ls=(n-1)*var(xsim)/qchisq(alfa/2,n-1)
if( sigma^2  >li&  sigma^2 < ls) conta=conta + 1
}
return(conta/Na)
}

cobertura.Var(1000,30,0.05,10,1)

cobertura.Var(1000,30,0.05,15,2)
cobertura.Var(1000,20,0.05,10,1)

cobertura.Var(1000,5,0.05,10,1)












