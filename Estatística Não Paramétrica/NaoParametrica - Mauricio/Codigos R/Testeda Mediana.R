#####CC0291-Estatística não Paramétrica:

###Notas de aula da Chang-Beti Kira!!!!!



####X=diâmetro do arame em mm

###hipótese: med=1



X=c(1017,1001,1008,995,1006,1011,1009,1009,1003,998,990,1007,1002)/1000;X

n=length(X);n

alfa=0,10#######



#####H_0: mediana=1 vs H_1: mediana diferente de 1.

#####H_0: teta=0,5 vs H_0: teta diferente de 0,5 .

####K=número de sinais positivos

###Se H_0 é verdade temos K \sim bin(13,1/2)

###Vamos construir a distribuição nula de K
 
k=0:n;k
pk=dbinom(k,n,1/2)
Pk=pbinom(k,n,1/2)
Sk=1-pbinom(k-1,n,1/2)

tab=cbind(k,pk,Pk,Sk)

round(tab,5)

Y=ifelse( X <1,0,1)
Y
table(Y)
Y=X-1;Y

K_obs=sum(Y);K_obs ###Número de sucessos na amostra.


s=K_obs



######Devemos escolher k_1 e k_2 tais que:



###P(K>=k_2)alfa/2=0,05 ou P(K<=k_1)=alfa/2=0,05 

###Na tabela o mais próximo é 0,04614
k_2=10;k_1=3



###RC={0,1,2,3,10,11,12,13}    RA={4,5,6,7,8,9}


###Logo k_obs pertence a região crítica , portanto rejeitamos H_0.


######Calcule o nível descritivo do teste.


p_1=1- pbinom(K_obs -1,n,1/2);p_1 
p_2=pbinom(K_obs ,n,1/2);p_2

teta_est=s/n;teta_est

nd=2*min(p_1,p_2);nd

alfa=0.10



###Fazer direto no R:binom.test




mod=binom.test(s,n,conf.level=0.90)
names(mod)

mod

###Note o nível descritivo e o IC(teta,90%). 0,5 não pertence ao IC.


###Como se constrói o IC????? olhem a programação do R.



###Este intervalo exato usa a distribuição a F

###IC(p,100(1-alfa)=[L_i,L_s], L_i=1/aux_1; L_s=1/aux_2

######aux_1= 1 + ((n-s+1)/s)*F{1-alfa/2,v_1,v_2}

v_1=2*(n-s+1);v_1  
v_2=2*s;v_2

f_1=qf(1-alfa/2,v_1,v_2);f_1

aux_1= 1 + ((n-s+1)/s)*f_1;aux_1

L_i=1/aux_1;L_i


######aux_2= 1 + ((n-s)/((s+1)*F{1-alfa/2,v_3,v_4})

v_3=2*(s+1);v_3
v_4=2*(n-s);v_4
f_2=qf(1-alfa/2,v_3,v_4);f_2


aux_2= 1 + (n-s)/((s+1)*f_2);aux_2
L_s=1/aux_2;L_s






####Esta construção é bem interessante!!!!!



####Vamos estudar!!!!!!


#####seção 9.2 do livro Estatística Básica de Daniel Furtado Ferreira.




####Note que este teste não leva em conta  a magnitude da diferença entre 

#### o valor e a mediana se H_0 é verdade.


###Vamos pensar num teste alternativo

Y=rep(1,n);Y 


wilcox.test(X,Y,paired=T,conf.level=0.90)


mu=n*(n+1)/4;mu

VV=n*(n+1)*(2*n+1)/24;VV
sigma=sqrt(VV);sigma

z_1=(72.5-mu)/sigma;z_1

p_1=pnorm(z_1);p_1


nda1=2*min(p_1,1-p_1);nda1


z_2=(72-mu)/sigma;z_2

p_2=pnorm(z_2);p_2


nda2=2*min(p_2,1-p_2);nda2


vmax=n*(n+1)/2;vmax
v=0:vmax;v
pv=dsignrank(v,n)
Pv=psignrank(v,n)
Sv=1-psignrank(v-1,n)
tab=cbind(v,pv,Pv,Sv);round(tab,5)



###Pedindo os quantis diretamente
 
P5=qsignrank(0.05,n);P5

psignrank(22,n) ####>0,05

psignrank(21,n)  #####<0,05

P95=qsignrank(0.95,n);P95

1-psignrank(69,n) ####### <0,05


1-psignrank(68,n) ####### >0,05


####RC= V<=22 ou V.=69 rejeitar H_0.


72.5>69  ####Rejeitar H_0:









































###Relação entre a binomial e a F




####Relação entre a beta e a binomial:


#Seja X ~Bin(n,p) , Y ~Beta(a=k,b=n-k+1)

###p_1=P(X>=k)=P(Y <=p)=p_2

###X~ Bin(13,1/2)
k=10;n=13;p=1/2

a=k;a;b=n-k+1
a;b

p_1=1-pbinom(9,13,1/2);p_1

p_2=pbeta(p,a,b);p_2



####Relação entre a beta e a F

alfa=0.10
s=10;n=13

v_1=2*(n-s+1);v_1
v_2=2*s;v_2


f_1=qf(1-alfa/2,v_1,v_2);f_1

aux1=1+((n-s+1)/s)*f_1;aux1

L_i=1/aux1;L_i





v_3=2*(s+1);v_3
v_4=2*(n-s);v_4


f_2=qf(1-alfa/2,v_3,v_4);f_2

aux2=1+(n-s)/((s+1)*f_2);aux2

L_s=1/aux2;L_s

















v1=2*(n-s);v1
v2=2*(s+1);v2

f_1=qf(0.05,v1,v2);f_1

f_2=qf(0.05,v2,v1);f_2

li=11/(11+3*f_2);li
ls=11/(11+3*f_1);ls
