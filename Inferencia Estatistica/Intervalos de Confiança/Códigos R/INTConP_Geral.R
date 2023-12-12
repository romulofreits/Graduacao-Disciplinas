

###CC0288Inferência I 12/05/23


##p=proporção de consumidores que preferem a marca A

####n=400 tamanho da amostra.

##s=240 número total de sucessos na amostra.

###gama=0,95 n´vel de confiança 

n=400;s=240;gama=0.95

z=qnorm((1+gama)/2);z

####install.packages("DescTools")

require("DescTools")

BinomCI(240, 400,method=eval(formals(BinomCI)$method))   # return all methods



#####A saída traz    16 tipos de IC para p.



#####A função binom.test traz o IC exato que é o Clopper -Pearson

binom.test(240,400)$conf.int 





prop.test(240,400,correct=FALSE)$conf.int  #####Wilson modificado


prop.test(240,400)$conf.int######Wilsoncc




###O nosso feito  de MV em sala de aula é de Wald.



p_est=s/n;p_est

e=z*sqrt(p_est*(1-p_est)/n);e


p_est+c(-1,1)*e


###O conservador é dado por:


e=z/sqrt(4*n);e

p_est+c(-1,1)*e


###Não aparece na saída!!!!!!





