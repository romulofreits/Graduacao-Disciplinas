#####CC0291-20/04/2023

####Exemplo 14.6 do Bussab&Morettin

###REste se os dados são oriundo de X \sim N(10,25)



#####Amostra

X=c(104,173,393,444,637,651,761,764,818,848,857,865,971,987,995,1001,1052,1069,
1172,1217,1261,1298,1303,1316,1411,1460,1464,1475,1668,2214)/100;X



n=length(X);n #####tamanho da amostra



###Análise Exploratória

xb=mean(X);xb
s2=var(X);s2
s=sd(X);s

summary(X)

boxplot(X)#####Comentar


stem(X)#####Comentar!!!!!!


stripchart(X,pch = 19,method = "stack",at = 0.1)

qqnorm(X)
qqline(X,col="red")  #####Comentar!!!!!!!!

##########Seria interessante calcular a assimetria e a curtose.




#####Vamos ao teste



####H_0: F=F_0( da Normal(10,25) vs H_1:Fdiferente de F_0


####Vamos fazer como no livro:



um=rep(1,n)

Fe=cumsum(um)/n

mu=10;sigma=5

F_0= pnorm(X,mu,sigma)


DM=abs(F_0-Fe)
i=1:n
tab=cbind(i,F,Fe, DM);round(tab,4)

Dmax=max(D);Dmax;round(Dmax,3) #####Bate com a resposta do livro


#####Vamos fazer direto no R:

ks.test(X,"pnorm",mu,sigma)


##Note que o valor da saída é o 0,11633. 

##Vamos explicar de Novo!!!!


Fem=c(0,Fe[1:(n-1)])  ####Acumulada por baixo!!!!!!



Dm=abs(F_0-Fem)

max(Dm);round(max(Dm),5)


tab=cbind(i,Fe,Fem,DM,Dm);round(tab,5)

WF=
Tab=cbind(DM,Dm)
Tab

WS=apply(Tab,1,max)
WS
Tab=cbind(Tab,WS);round(Tab,5)

