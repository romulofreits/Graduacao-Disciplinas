###CC0288-15/05/2023-prof.Mauricio



####O comprimento das peças produzidas  por uma máquina são normalmente 
####distribuídos. Uma amostra aleatória de 10 peças apresentou os seguintes 
####valores em milímetros:

##X comprimento da peça em milimetros

##X ~N(mu,sigma2)

 
X=c(875,872,873,876,878,874,873,877,874,872)/100;X


###X tem distribuição Normal?alfa=0,05

shapiro.test(X)
#Nivel de significancia
alfa=0.05
nd=0.4473###nível descritivo do teste ou p-value

alfa<nd ###Resposta SIM-Os dados são normais 
###caso contrário não se aplica o teste t.
boxplot(X,col='red')
qqnorm(X)
qqline(X,col='red')
hist(X, prob=TRUE)

##############################################################################

 
###Construa um intervalo de 96\% para o comprimento médio das peças produzidas 
##por esta maquina.


###Construa um intervalo de 98\% para a variância  das peças produzidas 
##por esta máquina.

n=length(X);n
SX=sum(X);SX;SX2=sum(X^2);SX2


###mu=comprimento médio das peças produzidas por esta maquina(populacional).
##sigma2= variância  das peças produzidas por esta máquina.


########Estimação pontual de mu:

Xb=SX/n;Xb;mean(X) 

########Estimação pontual de sigma2

s2=(SX2-SX^2/n)/(n-1);s2;var(X)

########Estimação pontual de sigma
s=sqrt(s2);s;sd(X)
round(s,4)




########Estimação intervalar  de mu com gama=0.96:
gama=0.96;alfa=1-gama;alfa/2

t_tab=qt(1-alfa/2,n-1);t_tab;round(t_tab,3)



e=t_tab*(s/sqrt(n));e

IC96=Xb +c(-1,1)*e;IC96
round(IC96,4)


#####Direto no R


t.test(X,conf.level=0.96)$conf.int



#####Intervalo de confiança para sigma^2


gama=0.98;alfa=1-gama;alfa/2

q_1=qchisq(alfa/2,n-1);q_1;round(q_1,3)
q_2=qchisq(1-alfa/2,n-1);q_2;round(q_2,3)

qchisq((1+gama)/2,n-1)

Num=(n-1)*s2;Num

li=Num/q_2;li
ls=Num/q_1;ls

IC98=c(li,ls)
IC98;round(IC98,4)

#Tabela da F
f2 = qf(0.95,3,5);f2;round(f2,2)
f1 = qf(0.05,3,5);f1;round(f1,2)
f0 = qf(0.95,5,3);f0;round(f0,2)
1/f0;f1






#Fator de Correção
n =27
p = 1/3
#P(X<=20)=p1

p1 = pbinom(20,n,p);p1
p2 = pbinom(15,n,p);p2
p3 = pbinom(10,n,p);p3
p4 = pbinom(9,n,p);p4
mu = n*p;mu
sigma = sqrt(n*p*(1-p));sigma

z = (9.5-mu)/sigma;z
pnorm(z);p4

