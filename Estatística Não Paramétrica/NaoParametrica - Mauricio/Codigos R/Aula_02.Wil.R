##CC0291-EstatÃ­stica NÃ£o ParamÃ©trica
###Prof: MaurÃ­cio Mota
###Aula:16/03/2023.
#####

## Suponha que uma nova técnica cirúrgica está sendo comparada com uma técnica
## padrão. A comparação será feita no tempo de recuperação dos pacientes

####Grupo Controle- Pop 1
 
C=c(20,21,24,30,32,36,40,48,94)
n=length(C);n

## Grupo: Tratamento -Pop 2

T=c(19,22,25,26,28,29,34,37,38)

m=length(T);m

###Se H_0 é verdade temos uma única população.

tempo=c(C,T)
tempo


####Vamos definir os grupos:

grupo=factor(rep(1:2,each=9));grupo

tempo_o=sort(tempo)###Ordenando a amostra geral.

tempo_o

tempo_r=rank(tempo) ###Calculando os postos.

tempo_r

###organizando um tabelão:



gr <- grupo[order(tempo)]
gr

dad=cbind(grupo,tempo,tempo_o,gr,tempo_r);dad


####Vamos calcular a soma dos postos por grupo.



aux=by( tempo_r,grupo,sum);aux

#####Soma dos postos do grupo tratamento

w_s=aux[2];w_s  ######

u_s= w_s-m*(m+1)/2;u_s  #####estatí­stica de Mann-Whithney

N=m+n;N
mu=n*m/2;mu
sigma2=n*m*(N+1)/12;sigma2
sigma=sqrt(sigma2);sigma

####ní­vel descritivo exato 

nd=pwilcox(31,m,n);nd

####ní­vel descritivo aproximado.

nda=pnorm(31,mu,sigma);nda #######aproximado



########Fazer direto no R: wilcox.test

##########?wilcox.test


#####H_0: delta=0 vs delta <0- menor tempo de recuperação:


teste=wilcox.test(T,C,alternative= "less" );teste

#####Vamos estudar  muito!!!!!!!!!!!!!!!!!!!



#####Análise  paramétrica!!!!!!!!

s2C=var(C);s2C
s2T=var(T);s2T


aux=max(s2C,s2T)/min(s2C,s2T);aux

aux>4 ####Variâncias distintas!!!!!!




#######Vamos discutir a normalidade!!!!!




require(nortest)

lillie.test(C)

lillie.test(T)


shapiro.test(C)


shapiro.test(T)



ad.test(C)

ad.test(T)


qqnorm(C)

qqline(C,col="red")  ####procurar material para interpretar corretamente!!!!!!!



qqnorm(T)

qqline(T,col="red")

mod1=t.test(T,C,alternative= "less" );mod1







#####Vamos corrigir a observação 94



C1=c(20,21,24,30,32,36,40,48,44)


T1=c(19,22,25,26,28,29,34,37,38)


s2C=var(C1);s2C
s2T=var(T);s2T


aux1=max(s2C,s2T)/min(s2C,s2T);aux1

aux1>4 ####Variâncias iguais!!!!!!






teste2=wilcox.test(T1,C1,var.igual=T,alternative= "less" );teste2  #######Comente!!!!!!





C1=c(20,21,24,30,32,36,40,48,44)


T1=c(19,22,25,26,28,29,34,37,38)


qqnorm(T1)

qqline(T,col="red")


mod2=t.test(T,C,alternative= "less" );mod2
