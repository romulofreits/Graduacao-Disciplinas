

###CC029 Estatistica Nao Parametrica- 15/05/23


##p=proporção de consumidores que preferem a marca A

####n=400 tamanho da amostra.

##s=240 número total de sucessos na amostra.

###gama=0,95 nível de confiança : IC(p,gama)

n=400;s=240;gama=0.95



z=qnorm((1+gama)/2);z

####install.packages("DescTools")

require("DescTools")

BinomCI(240, 400,method=eval(formals(BinomCI)$method))# retorna todos os metodos.



#####A saída traz    16 tipos de IC para p.



################################################################################


#####A função binom.test traz o IC exato que é o Clopper -Pearson

binom.test(240,400)$conf.int 

r_1=2*(n-s+1);r_2=2*s;r_1;r_2
r_3=2*(s+1);r_4=2*(n-s)
r_3;r_4

f_2=qf(1-alfa/2,r_1,r_2);f_2
f_1=qf(1-alfa/2,r_3,r_4);f_1

l_i=s/(s +(n-s+1)*f_2);l_i
l_s=(s+1)/(s+1 +(n-s)/f_1);l_s

BinomCI(240,400,method=c("clopper-pearson"))


#############################################################################


prop.test(240,400,correct=FALSE)$conf.int  #####Wilson modificado


prop.test(240,400)$conf.int######Wilsoncc


BinomCI(240,400,method=c("wilsoncc"))


################################################################




###O nosso feito  de MV em sala de aula é de Wald.



p_est=s/n;p_est

e=z*sqrt(p_est*(1-p_est)/n);e


p_est+c(-1,1)*e


BinomCI(240,400,method=c("wald"))








 ######################################################################


prop.test(240,400,)$conf.int  #####Wilson





BinomCI(240,400,method=c("wilson"))


#######################################################################

a=(s-0.5)/n;a
l_i=a- (z/sqrt(n))*sqrt(a*(1-a));l_i
 
b=(s+0.5)/n;b
l_s=b+ (z/sqrt(n))*sqrt(b*(1-b));l_s

###Este não aparece!!!!!


####################################################################


###O conservador é dado por:


e=z/sqrt(4*n);e

p_est+c(-1,1)*e


###Não aparece na saída!!!!!!





