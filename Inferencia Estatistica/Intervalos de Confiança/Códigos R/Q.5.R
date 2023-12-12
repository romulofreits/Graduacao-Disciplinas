# Questão 05 da prova de 2022.2 de Intervalos de Confiança
# As duas populações são DEPENDENTES

A = c(16,14,19,18,19,20,15,18,17,18) #método A
B = c(13,19,14,17,21,21,10,14,13,15) #método B
D = A - B
n = 10 #tamanho da amostra A e amostra B

#Criando uma tabela com os valores necessários
tabela = cbind(A,B,D,D^2);tabela
db = mean(D) # média de D
SD = sum(D); SD # somatório de D
S2D = sum(D^2); S2D  
s2_D = (S2D-SD^2/n)/(n-1);s2_D # desvio padrão amostral
s_D = sqrt(s2_D);s_D

t_tab = qt(0.975,n-1);t_tab;round(t_tab,3)
e = t_tab*s_D/sqrt(n);e
IC95 = db + c(-1,1)*e;IC95
#Como o IC95 contém o ponto zero não há diferenças entre os dois métodos

var.test(A,B)
?var.test

t.test(A,B)
t.test(A,B, var.equal = T)
t.test(A,B,paired = T) #As pop. sao dependenetes, por isso o teste                           correto a ser usado é o pareado


shapiro.test(A)
shapiro.test(B)
#Como 0,5733 > 0,05 e 0,4162 > 0,05 não podemos rejeitar a Normalidade das duas populações.
