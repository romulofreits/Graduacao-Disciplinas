##### CC0291 -Estatística não Paramétrica-29/06/2023- Prof. Mauricio

###As matrizes uniformes representam um caso importante na teoria etstística.
### Elas representam o caso em que as variáveis envolvidas possuem  variâncias
### iguais (a) e mesma covariância (b).
 

###Matriz Uniforme A  de ordem 2
p=2
a=4;b=1

A2=matrix(c(a,b,b,a),ncol=2);A2

detA2=(a-b)*(a+(p-1)*b);detA2;det(A2)

traA2=sum(diag(A2));traA2

IA2=solve(A2);IA2
require(MASS)
fractions(IA2)#####também é uma matriz uniforme com a=4/15 e b=-1/15

aux2=eigen(A2);aux2

rc=aux2$values;rc

### Cálculo do traco e do determinante de A2

traco=sum(rc);traco
det=prod(rc);det

L=diag(rc);L

C=aux2$vectors;C

####Propriedades

###
t(C)%*%C

round(t(C)%*%C,4)########Matriz identidade de ordem 2

round(C%*%t(C),4)########Matriz identidade de ordem 2


M=C%*%L%*%t(C);M ###Decomposição espectral de A


###Fatorar A=B*t(B)=C*L*t(C)=(C*RL)*(RL*t(C))

RL=sqrt(L);RL

B=C%*%RL;B

B%*%t(B);A ####Porreta, né!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

####Outra maneira 
#########?chol
D=chol(A);D #####B=t(chol(A))Decomposição de Cholesky..

t(D)%*%D



###Fazer uma função no R para gerar a matriz Ap uniforme a,b de ordem p



###Matrizes Especiais em Estatística

##1. Matriz Uniforme  M(i,i)=a, M(i,j)=b, i diferente de j

Munif=function(a,b,p){

A=matrix(b,p,p)
for (i in 1:p){
A[i,i]=a
}
A
}


Munif(4,1,2)####Matriz U2


##Exemplo 1.



Munif(10,2,4)

##Exemplo 2.

Munif(8,1,7)

##Exemplo 3.

Munif(4,1,3)

##Exemplo 4.

Munif(3,2,2)

##determinante de uma matriz uniforme
##|M|=(a-b)^{p-1}*(a +(p-1)*b)

det(Munif(4,1,3))
a=4;b=1;p=3;(a-b)^(p-1)*(a +(p-1)*b)



#####Inversa de uma matriz uniforme
require(MASS)
solve(Munif(3,2,2))


fractions(solve(Munif(3,2,2)))


###Note que a inversa também é uma matriz uniforme.
Munif(10,2,4)

solve(Munif(10,2,4))


fractions(solve(Munif(10,2,4)))


###Note que a inversa também é uma matriz uniforme.



##Fórmula Geral.

##A é U(a,b,p) então   IA é U(ia,ib) com

##ia=    e ib=


########################  função para calcular o traço.########################################



trac = function(matriz){
if(dim(matriz)[1] != dim(matriz)[2])
   {print("Matriz não é quadrada.")}
   else 
   {sum(diag(matriz))}
}

M1=diag(5) 
trac(M1)

M2=matrix(1,4,5);M2

trac(M2)


###############################################################################################

################Calcular o posto de uma matriz no R ############################################

############################################################################################


###Criação de uma  função!!!!!!

posto=function(M){
 aux=M%*%t(M)
 rc=eigen(aux)$values 
 aux1=round(rc,2) 
 aux2=aux1[aux1 >0]
 posto=length(aux2) 
 return(posto)
}

###Exemplo 1.
X=matrix(c(1,1,1,1,1,1,0,0,0,0,1,1),ncol=3);X

posto(X)

###Exemplo 2.
 A=matrix(c(4,2,2,2,2,0,2,0,2),ncol=3);A
det(A)
posto(A)


###Exemplo 3.
 U=matrix(c(3,1,1,1,3,1,1,1,3),ncol=3);U
posto(U)

###Exemplo 4.

 Y=matrix(c(2,3,5,4),ncol=1);Y
posto(Y)


##############Direto no R####################################

###O posto sai como um subproduto!!!!!!!!!!!

qr(X)###rank=2
qr(A)###rank=2

qr(U)###rank=3

qr(Y)###rank=1


#############################################################################################

###Entrada de matriz simétrica
 mat.sim = function(x, n) {
 A = matrix(0, n, n)
 A[row(A) >= col(A)] = x
 A + t(A) - diag(diag(A))
 }


 S1 = mat.sim(c(0.291, -0.001, 0.002, 0.01, 0.011,
 0, 0.003, 0.001, 0, 0.01), 4); S1



 S2 = mat.sim(c(0.561, 0.011, 0.001, 0.037, 0.025,
 0.004, 0.007, 0.005, 0.002, 0.019), 4);S2


 S3 = mat.sim(c(0.261, 0.03, 0.003, 0.018, 0.017,
 0, 0.006, 0.004, 0.001, 0.013), 4);S3

###############################################################################################




########################################Produtos Especiais####################################


########Produto de Kronecker

A=matrix(3,2,5);A
dim(A)
B=matrix(c(1,2,3,4,5,6),ncol=3);B
dim(B)
AB=kronecker(A,B);AB
dim(AB)


BA=kronecker(B,A);BA
dim(BA)

kronecker(A,t(B))

##############################################################################

C=matrix(5,2,5);C

dim(A)==dim(C)

AC=A*C;AC ####produto elemento a elemento!!!!!!!!!!!!!!!!

AdC=A/C;AdC

CdA=C/A;CdA

fractions(CdA)



###############################################################################


######EXercício 2.30-página 112-JW

####X=(X_1,X_2,X_3,X_4)

muX=matrix(c(4,3,2,1),ncol=1);muX

rownames(muX)=c("X1","X2","X3","X4")
muX
SIGX=matrix(c(3,0,2,2,0,1,1,0,2,1,9,-2,2,0,-2,4),ncol=4);SIGX

rownames(SIGX)=c("X1","X2","X3","X4")
colnames(SIGX)=c("X1","X2","X3","X4")

SIGX


####Determine a Variância Total.

VT=sum(diag(SIGX));VT


####Determine a Variância Generalizada.

VG=det(SIGX);VG


####Partição de X-------X^t=[(X1,X_2)^t| (X3,X_4)^t)=[X^(1)| X^(2)


A=matrix(c(1,2),ncol=2);A

B=matrix(c(1,2,-2,-1),ncol=2);B


#####Considere combinações lineares Y_1=AX(1) e (Y_2,Y_3)=BX(2)

######Achar:

####item a: E(X^(1)), X^(1)=A1*X

A1=matrix(c(1,0,0,1,0,0,0,0),nrow=2);A1  


muVX1=A1%*%muX;muVX1 


####item b: E(A X^(1)), X^(1)=A1*X


muY1=A%*%muVX1;muY1


####item c: Cov(X^(1)), Cov(X^(1))=A1*Cov(X)A1^t


COVVX1=A1%*%SIGX%*%t(A1);COVVX1



####item d: Cov(AX^(1)), Cov(X^(1))=A1*Cov(X)A1^t


COVVX1=A1%*%SIGX%*%t(A1);COVVX1

VY1=A%*%COVVX1%*%t(A);VY1



####item e: E(X^(2)), X^(2)=A2*X

A2=matrix(c(0,0,0,0,1,0,0,1),nrow=2);A2  


muVX2=A2%*%muX;muVX2 


####item f: E(B X^(2)), X^(2)=A2*X


muY=B%*%muVX2;muY


####item g: Cov(X^(2)), Cov(X^(2))=A2*Cov(X)A2^t


COVVX2=A2%*%SIGX%*%t(A2);COVVX2



####item h: Cov(BX^(2)), Cov(BX^(2))=B*Cov(X^(2))B^t




COVVY2=B%*%COVVX2%*%t(B);COVVY2



####item i: Cov(X^(1),X^(2))=Cov( A1X,A2X=A1Cov(X) A2^t



COVVX1VX2=A1%*%SIGX%*%t(A2);COVVX1VX2


######item j: Cov(AX^(1),B X^(2))=Cov(A A1X,B A2X))= A A1 Cov(X)(BA2)^t)
#######AA1*SigX*A2^t*B^t


A%*%A1%*%SIGX%*%t(A2)%*%t(B)