####Aula de Inferência-dia05/05/2023

##Capítulo 5-Heleno e Monica-Intevalo de confiança


###Tabela 5.1= n=20 observações de U ~(0,1)


U=c(659,469,353,847,591,17,594,749,381,128,51,535,658,328,757,700,12,166,45,781)/1000
length(U)
tab5.1=matrix(U,ncol=5);tab5.1


##X-exp(teta=2), X=(-1/teta)*Log(1-U)


teta=2 

X=-(1/teta)*log(1-U)

tab5.2=matrix(X,ncol=5);round(tab5.2,4)


###Note qu temos valores diferentes.


##No livro temos 0,9008 e é 0,0908.



S20=sum(X);S20;round(S10,4)
S10=sum(X[1:10]);S10;round(S10,4)

#####Sabemos que Q=2teta*S10~qui^2(20)

alfa=0.05
q_1=qchisq(alfa/2,20);q_1;round(q_1,2)
q_2=qchisq(1-alfa/2,20);q_2;round(q_2,2)


f=function(q) dchisq(q,20)

plot(f,20,0,50)
abline(h=0,col="red")


#   ##Intervalo de confiança com n=10.

l_1=q_1/(2*S10);l_1
l_2=q_2/(2*S10);l_2

ICteta95=c(l_1,l_2);ICteta95;round(ICteta95,2)


C_1=l_2-l_1;C_1

#####Sabemos que Q=2teta*S20~qui^2(40)


q_1=qchisq(alfa/2,40);q_1;round(q_1,2)

q_2=qchisq(1-alfa/2,40);q_2;round(q_2,2)

###Intervalo de confiança com n=20.

l_1=q_1/(2*S20);l_1
l_2=q_2/(2*S20);l_2

ICteta95=c(l_1,l_2);ICteta95;round(ICteta95,2)


C_2=l_2-l_1;C_2


C_1;C_2
C_1<C_2








######Aproximação pela normal

k=40
EY=k; VY=2*k;EY;VY

z_1=qnorm(alfa/2);z_1;round(z_1,2)

z_2=qnorm(1-alfa/2);z_2;round(z_2,2)

q_1a=EY +z_1*sqrt(VY); q_1a;q_1

q_2a=EY +z_2*sqrt(VY); q_2a;q_2  #####Comparar e comentar!!!!!!!



######Exemplo 5.2.2  X \unif (0,teta)

###Y_n=max(X_1,X_2,...,X_n) é uma estatistica suficiente para teta



####Seja Q=Y_n/teta ~Beta(a=n,b=1) é uma quantidade pivotal 

###A acumulada de Q é dada por G(q)=q^n




##G(q_1)=alfa/2-------:q_1^n=alfa/2.......>q_1=(alfa/2)^{1/n}


##G(q_2)=1-alfa/2-------:q_2^n=1-alfa/2.......>q_2=(1-alfa/2)^{1/n}




X=c(659,591,381,658,12,469,17,128,328,166)/1000;X

yn=max(X);yn
alfa=0.05
n=10
q_1=(alfa/2)^{1/n};q_1
   
q_2=(1-alfa/2)^{1/n};q_2


l_1=yn/q_2;l_1

l_2=yn/q_1;l_2


ICteta95=c(l_1,l_2);ICteta95

round(ICteta95,3)


####Fazendo com n=20
X=U
n=20;yn=max(X);yn


q_1=(alfa/2)^{1/n};q_1
   
q_2=(1-alfa/2)^{1/n};q_2


l_1=yn/q_2;l_1

l_2=yn/q_1;l_2


ICteta95=c(l_1,l_2);ICteta95

round(ICteta95,3)




#####Gere as amostras diretamente do R:


### X-sim Exp(teta=2)


teta=2
set.seed(32)
X=rexp(20,2);X


##Estimativa pontual de teta

teta_est=1/mean(X);teta_est
S=sum(X);S

alfa=0.05


##Q=2*teta*S ~qui-quadrado(40)




q_1=qchisq(alfa/2,40);q_1;round(q_1,2)

q_2=qchisq(1-alfa/2,40);q_2;round(q_2,2)

###Intervalo de confiança com n=20.

l_1=q_1/(2*S);l_1
l_2=q_2/(2*S);l_2

ICteta95=c(l_1,l_2);ICteta95;round(ICteta95,2)  ###Note quw teta=2 pertence ao IC.



C=l_2-l_1;C




