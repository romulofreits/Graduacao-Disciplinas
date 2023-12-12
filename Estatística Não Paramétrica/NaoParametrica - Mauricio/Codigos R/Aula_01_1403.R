


####Exercício da aula do dia 14/03/2023

C=c(20,21,24,30,32,36,40,48,94)

m=length(C);m

T=c(19,22,25,26,28,29,34,37,38)



n=length(T);n
N=m+n;N


Aux=c(C,T);Aux



w_min=m*(m+1)/2;w_min

W_obs=76####soma dos postos do grupo tratamento

U_obs=W_obs-w_min;U_obs  ####Veja na saída do R:

###Fazer um programa no R para calcular a soma dos postos do grupo tratamento


Postos=order(Aux);Postos

#####?wilcox.test-- Estudar a sintaxe!!!!!!!


?wilcox.test
teste=wilcox.test(T,C);teste


#####nd=P(W>=31)=1-P(W<=30)

p_1=1-pwilcox(30,m,n);nd ;p_1

p_2=pwilcox(31,m,n);p_2

nd=2*min(p_1,p_2);nd

round(nd,4)


EWs=m*(N+1)/2;EWs

EUs=m*n/2;EUs

VUs=n*m*(N+1)/12;VUs


u=0:(m*n);u

pu=dwilcox(u,m,n)

sum(pu)

EU=sum(u*pu);EU

VarU=sum( (u-EU)^2*pu);VarU






