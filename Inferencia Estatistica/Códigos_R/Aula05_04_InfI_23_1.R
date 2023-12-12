####Primeira Aula do R-Prof.Mauricio-05/04/23

###X \sim Bin(m=5,p)

x=0:5;x ####Suporte de X

f=c(1,7,29,32,24,7); f #####Valores amostrais.
 

#cálculo do tamanho da amostra 

n = sum(f);n # Somando todos os valores da amostra

####Fazer uma tabela por linha.

tab1 = rbind(x,f); tab1


####Fazer uma tabela por coluna.


tab2 = cbind(x,f); tab2

####Vamos calcular a média e a variância amostral.



xf = x*f
x2f = x^2*f


####ampliando a tabela 2.


tab2 = cbind(x,f,xf,x2f); tab2

#cálculo da média da amostra 
sx = sum(xf); sx
xb = sx/n; xb
m = 5; m #parâmetro coonhecido da binomial
p_est = xb/m; p_est


#vaamos começar!  Fazer a mostra com n=100 elementos:
 
A = rep(x,f); A

table(A)

#De volta ao inicio 


# Simular uma amostra de tamanho 100 da binomial n= 5 p=0,6.


set.seed(32)###A semnete de cada aluno se=á seu número de matrícula!!!!!
 
n = 100;m=5; p=0.6;

Ag = rbinom(n,m,p); Ag
sort(Ag)####Faz o rol do menor para o maior  valor:

table(Ag);table(A)


# Cálculo da média com os dados originais;


####mu=E(X)=m*p, sigma2=m*p*(1-p)
 
mean(Ag); mu = m*p; mu
s2 = var(Ag); sigma2 = m*p*(1-p); s2; sigma2

s = sd(Ag); sqrt(s2); s
 
# Teste se os dados seguem uma bin m = 5 e  p = 0,6.

####### Gere a função de probabilidade de X


px = dbinom(x, m, p); tab3 = cbind(x, px)
sum(px)
 tab3

####Calcula F(x)=P(X<=x)
Px=pbinom(x, m, p)

###Sx=P(X>=x)=P(X=x) +P(X>x)=P(X=x)+ 1- F(x)=P(X=x)+ 1-Px

Sx=px+1-Px


tab3 = cbind(x, px,Px,Sx)

tab3

###Quanto vale  b de sorte que P(X<=b)=0.66304?


b=qbinom(0.66304,m,p);b  #####DE acordo com a tabela 3!!!!!



#####Calcule E(X),E(X^2),Var(X)!!!!!!


EX = sum(x*px); EX ####Primeiro momento em relação à origem!!!!!


EX2 = sum(x^2*px); EX2#####Segundo momento em relação à origem!!!!!
VX = EX2 - (EX)^2; VX#####Segundo momento  central.
sigma=sqrt(VX);sigma

###############################################################################

# Frequências observadas
fo = f; fo

# Frequências esperadas
fe = n*px; tab4 = cbind(x, fo, fe); tab4








# As frequências observadas estão perto das esperadas?




# Exemplo da Poisson do Morretin e Bussab.


x = 0:8; x
fo = c(200, 152, 60, 30, 13, 9, 7, 5, 4)
# Qual o tamanho da amostra
n = sum(fo); n
A = rep(x, fo); A
xb = mean(A); xb
table(A)
var(A)
floor(xb)
ceiling(xb)
floor(7.9); floor(-7.9)
set.seed(428897); A = rpois(480, 1.1833); A
table(A)
mean(A); var(A)
