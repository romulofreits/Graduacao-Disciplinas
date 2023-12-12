####CC02888-Inferência I-08/05/2023


####Treinar a Normal Usando  o R


####Faça o gráfico da fdp da N(0,1)


plot(function(x) dnorm(x,0, 1),-3, 3,xlab="z",ylab="Densidade da N(0,1)")
abline(h=0,col="blue")





####  Mostre que , na Normal Padrão, a área sob a curva vale 1.

##Maneira 1

gz=function (z) (sqrt(2*pi))^(-1)*exp(- z^2/2) 

I=integrate(gz,-Inf,Inf);I$value

##Maneira 2

gz2=function (z)  dnorm(z)

I1=integrate(gz2,-Inf,Inf);I1$value

##Maneira 3
integrate( function (z)dnorm(z),-Inf,Inf)$value


############Calcule E(Z);E(Z^2);E(Z^3);E(Z^4)

EZ=integrate( function (z) z*dnorm(z),-Inf,Inf)$value;EZ

EZ2=integrate( function (z) z^2*dnorm(z),-Inf,Inf)$value;EZ2


EZ3=integrate( function (z) z^3*dnorm(z),-Inf,Inf)$value;EZ3



EZ4=integrate( function (z) z^4*dnorm(z),-Inf,Inf)$value;EZ4



##Faça uma função para gerar os quantis de ordem p da N(0,1)


QN=function(p){
z_p=qnorm(p)
return(z_p)
} 

QN(0.5)  ######A mediana da N(0,1).

QN(0.25)  ###### O primeiro quartil da N(0,1).


QN(0.75)  ###### O terceiro quartil da N(0,1).


QN(0.95)  ###### O Percentil de ordem 95  da N(0,1).

QN(0.975)



########## Calcule p= P(-1 < Z < 1)=F(1)-Fi(-1)

p=integrate( function(z) dnorm(z),-1,1)$value;p

#####Ou

pnorm(1)-pnorm(-1)




#####Exemplo 2:  X~N(100,100)




mu=100;sigma=10

###Faça o gráfico da f.d.p. de X.

##Como o suporte é real para fazer o gráfico use Li=mu- 3*sigma; Ls=mu+3*sigma

Li=mu-3*sigma; Ls=mu+3*sigma
c(Li,Ls)

##Obs: P( mu-3*sigma < X <mu+3*sigma)=P(-3 < Z <3)=Fi(3)-Fi(-3)

pnorm(3)-pnorm(-3)   #### 99,73% dos valores de X estão no intervalo [Li,Ls].


curve(dnorm(x,mu,sigma), Li,Ls,ylab="f(x)",main=" X~N(100,100)")
abline(h=0,col="blue")



######Faça o gráfico da Acumulada de X.

curve(pnorm(x,mu,sigma), Li,Ls,ylab=" F(x)",main="  Acumulada da X~N(100,100)")
abline(h=c(0,1),col="blue")



######Faça o gráfico da  Sobrevivência  de X.

curve(pnorm(x,mu,sigma,lower.tail=F), Li,Ls,ylab=" S(x)",main="  Sobrevivência da X~N(100,100)")
abline(h=c(0,1),col="blue")



###Calcule as ordenadas da f.d.p. da N(100,100), nos pontos:70,80,90,100,110,120


x=seq(70,130,10);x

fx=dnorm(x,mu,sigma);fx

cbind(x,fx)

## Calcule a: pa=P(X <120)=P(Z< 2) e faça um gráfico marcando a região.  

z_a=(120-mu)/sigma;z_a


curve(dnorm(x,mu,sigma), Li,Ls,ylab="f(x)",main=" X~N(100,100)")
abline(h=0,col="blue")

polygon(c(70,seq(70,120,l=30),120),c(0,dnorm(seq(70,120,l=30),mu,sigma),0),
density=10,col="red")







p_a=pnorm(z_a);pnorm(115,mu,sigma);p_a;round(p_a,4)


######Calcule P(X >=80)=P(Z >z_b)


curve(dnorm(x,mu,sigma), Li,Ls,ylab="f(x)",main=" X~N(100,100)")
abline(h=0,col="blue")

polygon(c(80,seq(80,130,l=30),80),c(0,dnorm(seq(80,130,l=30),mu,sigma),0),
density=10,col="red")


z_b=(80-mu)/sigma;z_b



pb=pnorm(80,100,10,lower.tail=F);round(pb,4)

1-pnorm(z_b);pnorm(z_b,lower.tail=F)




### pc=P(|X-100|<=10)=P(|(X-100)/10 <=1)=P(|Z|<=1)=P(-1<Z<1)



###Outra Solução:  P(|X-100|<=10)=P(-10 <= X-100 <= 10)=P(90 <= X <= 110)=
 ##P(X<=110)-P(X<=90)




curve(dnorm(x,mu,sigma), Li,Ls,ylab="f(x)",main=" X~N(100,100)")
abline(h=0,col="blue")
polygon(c(90,seq(90,110,l=30),110),c(0,dnorm(seq(90,110,l=30),mu,sigma),0),
density=10,col="red")

x1=90;x2=110
zc1=(x1-mu)/sigma;zc1
zc2=(x2-mu)/sigma;zc2


pc=pnorm(1)-pnorm(-1);round(pc,4)


pc1=pnorm(110,mu,sigma)-pnorm(90,mu,sigma);round(pc1,4)




#####  Exemplo 3: Exemplo de Aplicação 1:

##X_1=peso bruto da lata -------X_2=peso da lata vazia


##X_1 ~N(mu1,sigma1^2), X_2 ~N(mu2,sigma2^2),PL=X_2-X_1

mu1=1000;sigma1=25;mu2=90;sigma2=8

muPL=mu1-mu2;muPL

sigma2PL=sigma1^2+ sigma2^2;sigma2PL

sigmaPL=sqrt(sigma2PL);sigmaPL


####Item a:P(PL <= 870)=P(Z <=z_a)

 z_a=(870-muPL)/sigmaPL;round(z_a,2)


curve(dnorm(x), -3,3,ylab="g(z)",main=" Z~N(0,1)")
abline(h=0,col="blue")
polygon(c(-3,seq(-3,-1.52,l=30),-1.52),c(0,dnorm(seq(-3,-1.52,l=30)),0),density=10,col="red")






pa=pnorm(z_a);pa;pnorm(870,muPL,sigmaPL);round(pa,4)






####Item b:P(PL > 900)=P(Z >=z_b)

 z_b=(900-muPL)/sigmaPL;round(z_b,2)

pb=pnorm(z_b,lower.tail=F);pb;pnorm(900,muPL,sigmaPL,lower.tail=F);round(pb,4)





################################################################################

#####Exemplo de aplicação 2

####X_1,X_2,....,X_n ~iid X~N(10,4), X_i peso do produto i, i=1,2,...n,(em gramas).
 ### W= Peso da caixa vazia- N(500,625),( em gramas)

##Y=peso da caixa cheia com n=50 produtos ( em gramas)

###Y=W + X_1+ X_2 +....+X_n, E(Y)=E(W) +E(X_1)+E(X_2) +....+E(X_n)=E(W) + n*E(X)

###Var(Y)=Var(W) + Var(X_1+X_2 +....+X_n),pois W é independente de X_1+X_2 +....+X_n.

###Var(Y)=Var(W) + Var(X_1)+Var(X_2)+....+Var(X_n=)Var(W)+n*var(X),

####X_1,X_2,....,X_n são independentes.


EW=500;VW=25^2
EX=10;VX=4
n=50 #### 50 produtos na caixa.

EY=EW +n*EX;EY

VY=VW + n*VX;VY

sigmaY=sqrt(VY);sigmaY

###Calcule a probabilidade de uma caixa cheia pesar mais de 1050 g

###Seja p=P(Y >1050)=P(Z> z)

z=(1050-EY)/sigmaY;round(z,2);z

p=pnorm(z,lower.tail=F);pnorm(1050,EY,sigmaY);p;round(p,4)

Li=EY-3*sigmaY;Li

Ls=EY+3*sigmaY;Ls

curve(dnorm(x,EY,sigmaY), Li,Ls,ylab="f(y)",xlab="y",main=" Y~N(1000,825)")
abline(h=0,col="blue")
polygon(c(1050,seq(1050,1100,l=30),1050),c(0,dnorm(seq(1050,1100,l=30),EY,sigmaY),
0),density=10,col="3")




###### Faça uma função no R para calcular a moda da Normal( Mo=mu)

##Fonte: Curso de R. autores: Ana Maria e José Roberto.(pg 59)


Moda_Normal=function(mu,sigma){
fn=function (x){ dnorm(x,mu,sigma)}
op=optimize(fn,c(mu-3*sigma,mu+3*sigma),maximum=T)
return(round(op$maximum,1))
}

Moda_Normal(0,1)   ######Mo=mu

Moda_Normal(0,2)   ######Mo=mu


Moda_Normal(10,1)   ######Mo=mu

Moda_Normal(10,2)   ######Mo=mu


Moda_Normal(100,10)   ######Mo=mu

Moda_Normal(100,20)   ######Mo=mu



###### Faça uma função no R para calcular o quantil de ordem p da Normal



QN=function(p,mu,sigma){
x_p=qnorm(p,mu,sigma)
return(x_p)
} 

QN(0.5,100,10)  ######A mediana da N(100,100).

QN(0.25,100,10)  ###### O primeiro quartil da N(100,100).


QN(0.75,100,10)  ###### O terceiro quartil da N(100,100).


QN(0.95,100,10)  ###### O Percentil de ordem 95  da N(100,100).

QN(0.975,100,10)






################# Y~Bin(n=15,p=0.4)

### pe=P(Y=4) exata

n=15;p=0.4;n;p

pe=dbinom(4,n,p);round(pe,4)

### pa=P( 3,5 <= X <=4,5)

a=4;mu=n*p;sigma2=n*p*(1-p);a;mu;sigma2 
sigma=sqrt(sigma2);round(sigma,2)

##Condições para a aproximação

n*p; n*p >5 ####Verdade!!!!

n*(1-p); n*(1-p) >5 ####Verdade!!!!
z_1=(a-0.5 -mu)/sigma;z_1

p1=pnorm(z_1);p1

z_2=(a+0.5 -mu)/sigma;z_2

p2=pnorm(z_2);p2


pa=p2-p1;pa

pe;pa  ####### Com duas casas decimais elas batem!!!!!!!


######pe=P(7 <= Y<=9), pa=P(7,5<= Y<=9,5)

pe=dbinom(7,n,p) +dbinom(8,n,p)+dbinom(9,n,p);pe

pbinom(9,n,p)-pbinom(6,n,p) ###Outra maneira de calcular pe

a=7;b=9

z_1=(a-0.5 -mu)/sigma;z_1

p1=pnorm(z_1);p1

z_2=(b+0.5 -mu)/sigma;z_2

p2=pnorm(z_2);p2


pa=p2-p1;pa


pe;pa  ####### Bem próximas. Com duas casas decimais elas batem!!!!!!!



###Exemplo do Bussab&Morettin
n=10;p=1/2;> 
> ######pe=P(7 <= Y<=9), pa=P(7,5<= Y<=9,5)
> 
> pe=dbinom(7,n,p) +dbinom(8,n,p)+dbinom(9,n,p);pe
[1] 0.3563535
> 
> pbinom(9,n,p)-pbinom(6,n,p) ###Outra maneira de calcular pe
[1] 0.3563535
> 
> a=7;b=9
> 
> z_1=(a-0.5 -mu)/sigma;z_1
[1] 0.2635231
> 
> p1=pnorm(z_1);p1
[1] 0.6039263
> 
> z_2=(b+0.5 -mu)/sigma;z_2
[1] 1.844662
> 
> p2=pnorm(z_2);p2
[1] 0.9674566
> 
> 
> pa=p2-p1;pa
[1] 0.3635303
> 
> 
> pe;pa  ####### Bem próximas. Com duas casas decimais elas batem!!!!!!!
[1] 0.3563535
[1] 0.3635303
> 


> 
> ######pe=P(7 <= Y<=9), pa=P(7,5<= Y<=9,5)
> 
> pe=dbinom(7,n,p) +dbinom(8,n,p)+dbinom(9,n,p);pe
[1] 0.3563535
> 
> pbinom(9,n,p)-pbinom(6,n,p) ###Outra maneira de calcular pe
[1] 0.3563535
> 
> a=7;b=9
> 
> z_1=(a-0.5 -mu)/sigma;z_1
[1] 0.2635231
> 
> p1=pnorm(z_1);p1
[1] 0.6039263
> 
> z_2=(b+0.5 -mu)/sigma;z_2
[1] 1.844662
> 
> p2=pnorm(z_2);p2
[1] 0.9674566
> 
> 
> pa=p2-p1;pa
[1] 0.3635303
> 
> 
> pe;pa  ####### Bem próximas. Com duas casas decimais elas batem!!!!!!!
[1] 0.3563535
[1] 0.3635303
> 





################################Exemplo do Bussab&Morettin

###P(Y>=7)=1- P(Y<=6)

n=10;p=1/2


###Primeira condição

n*p;n*p >5 


####Segunda condição:



n*(1-p);;n*(1-p) >5 


### As duas condições não estão satisfeitas!!!!!


##Mas assim mesmo vamos aproximar pela normal!!!!!


mu=n*p;mu

sigma2=n*p*(1-p);sigma2
sigma=sqrt(sigma2);sigma


pe=1-pbinom(6,n,p);pe;round(pe,3) ###bate com a resposta do livro.

##Vamos aproximar? P( X>=6,5)
a=7;a

z=(a-0.5-mu)/sigma;z

pa=1-pnorm(z);pa

pe;pa  ####Bem próximas.




###P( 3 <Y<=6)=F(6)-F(3)=p2-p1

p2=pbinom(6,n,p);p2

p1=pbinom(3,n,p);p1

pe=p2-p1;pe

###P( 3 <Y<=6)=P(4<=Y<=6))---------P(3,5 <= X <= 6,5) 

a=4;b=6

z_1=(a-0.5-mu)/sigma;z_1
z_2=(b+0.5-mu)/sigma;z_2

pa=pnorm(z_2)-pnorm(z_1);pa ###O livro traz a resposta 0,653 .Eles arredondaram

##0,94868 como 0,94 e não 0,95!!!

pnorm(0.95)-pnorm(-0.95) #### que bate com a nossa solução!!!!!

pe;pa  ####Bem próximas.

#####A binomial com p=1/2 é simétrica. Isto ajuda na aproximação!!!!

