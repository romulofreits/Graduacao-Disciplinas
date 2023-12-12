##CC288-INFERENCIA ESTATISTICA I -08/05/2023


###Profs: Ronald e Maurício.



##Estudar a natureza dos intervalos de confiança usando a quantidade pivotal

###### Q= 2*teta*S ~qui^2(2*n)!!!!!!!

##IC(teta,95%)-X~exp(teta) baseado em uma amostra aleatória de tamanho 20

##Simulação: retirar 100 amostras de tamanho 20 de X-exp(2)e construir 100 intervalos de confiança e contar


## Quantos intervalos contém teta=2?.

set.seed(32) ###Para garantir a reprodução dos resultados.

a=rexp(2000,2)##Gerar uma amostra de tamanho 2000=100 *20 da exp(2).

b=matrix(a,ncol=100)##b é uma matriz 20x100 , cada coluna uma amostra de tamanho 20. 


m=colMeans(b) ## médias amostrais das 100 amostras


tetaest=1/m;tetaest ##vetor das estimativas de teta de cada amostra(MV)


summary(tetaest);var(tetaest)## estatisticas descritivas

##IC[teta,95%]=[q1/(2*n*med),q2/(2*n*med],med=media amostral

##q=[q1,q2];F(q1)=0.025;F(q2)=0.975

q=cbind(qchisq(0.025,40),qchisq(0.975,40));q

IC=cbind(q[1]/(40*m),q[2]/(40*m));IC

c(which(IC[,1]>2))

n1=length(c(which(IC[,1]>2)));n1

c(which(IC[,2]<2))

n2=length(which(IC[,2]<2));n2
n=nrow(IC)
n1+n2

conf=100*(100 -n1-n2)/n;conf


###Tirar agora 1000 amostras de tamanho 20

set.seed(32)
a=rexp(20000,2)
b=matrix(a,ncol=1000)
m=colMeans(b)
tetaest=1/m

IC=cbind(q[1]/(40*m),q[2]/(40*m));IC

c(which(IC[,1]>2))

n1=length(c(which(IC[,1]>2)));n1

c(which(IC[,2]<2))

n2=length(which(IC[,2]<2));n2

n=nrow(IC);n
n1+n2

conf=100*(n -n1-n2)/n;conf



######Comentar!!!1


####Fazer de maneira mais elegante!!!!!!!

