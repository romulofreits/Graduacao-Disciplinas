###CC0291-Inferência não Paramétrica

###Exemplo 1:Armitage-1971) Cinquenta amostras de saliva foram colocadas em duas culturas 
###diferentes A e B. O objetivo dos tratamentos é a detecção do bacilo causador
## da tuberculose
###. Os resultados foram:

saliva=matrix(c(20,2,12,16),ncol=2,
dimnames = list(" Cultura A" = c("Sim", "Não"),
                       " Cultura B" = c(" Sim", "Não")))
 




saliva


##p_1=prob de detectar o bacilo da tuberculose usando o meio de cultura A:

##p_2=prob de detectar o bacilo da tuberculose usando o meio de cultura B:


##H_0:p_1=p_2  vs H_0:p_1 /=p_2  


alfa=0.05


qui_tab=qchisq(1-alfa,1);qui_tab


#####RC: Se qui-_cal >3,841 rejeitar H_0:



 mod1=mcnemar.test(saliva);mod1
mod1
names(mod1)


qui_cal=mod1$statistic;qui_cal #####Como se tira a mensagem????



qui_cal >qui_tab #####Rejeitar H_0:



nd_mod1=mod1$p.value;nd_mod1

mod2=mcnemar.test(saliva,correct=F)
mod2

nd_mod2=mod2$p.value;nd_mod2

a=20;b=12;c=2;d=16

n_1=b+c;n_1


qui_cal_cor=(abs(b-c)-1)^2/(b+c);qui_cal_cor

qui_cal=(b-c)^2/(b+c);qui_cal

qui_tab=qchisq(0.95,1);qui_tab

qui_cal>qui_tab


###Fazer pela Binomial

n_1=b+c;n_1

##H_0=p=1/2 vs H_1: p diferente de 1/2.


##Nível descritivo: nd=2*min(P(C <=2),P(C>2))=2*min(p1,p2)

p1=pbinom(2,n_1,1/2);p1

p2=1-pbinom(1,n_1,1/2);p2

nd_e=2*min(p1,p2)
nd_e;nd_mod1;nd_mod2






#####Exemplo 2- Manual do R:


## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.

Performance <-
matrix(c(794, 86, 150, 570),
       nrow = 2,
       dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings

a=784;b=150;c=86;d=570

num=(b-c)^2;num

q_cal=num/(b+c);q_cal
q_cal_corr=( abs(b-c)-1)^2/(b+c)

mcnemar.test(Performance,correct=F)

round(q_cal_corr,3)

###Exemplo 3: Conover-pags 168 e 169.



debate=matrix(c(63,21,4,12),ncol=2,
dimnames = list(" Antes" = c("Democrata", "Republicano"),
                       " Depois" = c(" Democrata", "Republicano")))
debate

mcnemar.test(debate)
mcnemar.test(debate,correct=F)



qui_cal=(21-4)^2/(21+4);qui_cal

qui_tab=qchisq(0.95,1);qui_tab

qui_cal>qui_tab
