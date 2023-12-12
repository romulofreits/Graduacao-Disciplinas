
### Elipsóide:###########  

### Livro do Johnson e Wichern, 6a edição:
### Pag: 239, Exemplo 5.8 - T2 de Hotelling e Elipses, Capítulo 8.
### Dados Tabela 5.8

## 3 FORMAS DE CONSTRUÇÃO DAS ELIPSÓIDES DE CONFIANÇA:

rm(list=ls(all=TRUE))

install.packages("MVTests")
require(MVTests)
install.packages("jocre")
require(jocre)
require(lattice)

setwd("C:/Users/WIN10/Downloads/Aulas REMOTAS 2021.1/Análise Multivariada")
dados=data.frame(read.csv(file.choose(), header=TRUE, sep=";"))

head(dados)
dim(dados)
attach(dados)
names(dados)

data=data.frame(dados)
attach(data)

############ FORMA 1: 

## função: dataEllipse{car}

install.packages("car")
library(car)

head(data)
plot(data[,1], data[,2])
plot(data, pch=19)


### CUIDADO COM OS LIMITES DOS EIXOS DE  CADA ELIPSÓIDE!!

par(mfrow=c(1,3))
dataEllipse(data[,1], data[,2], levels=c(0.90, 0.95, 0.99),
            ellipse.label=c(0.90, 0.95, 0.99),
            center.pch=19, lty=2, fill=TRUE, fill.alpha=0.1,
            xlim=c(1000,6000), ylim=c(-3000,6000), grid=TRUE,
            xlab="LegalAppeara", ylab="Extraordinary",
            main="LegalAppeara vs Extraordinary")

dataEllipse(data[,1], data[,3], levels=c(0.95),
            ellipse.label=c(0.95),
            center.pch=19, lty=2, fill=TRUE, fill.alpha=0.1,
            xlim=c(1500,6000), ylim=c(-1000,6500),
            grid=TRUE,
            xlab="LegalAppeara", ylab="Holdover",
            main="LegalAppeara vs Holdover", col=2)

dataEllipse(data[,2], data[,3], levels=c(0.95),
            ellipse.label=c(0.95),
            center.pch=19, lty=2, fill=TRUE, fill.alpha=0.1,
            xlim=c(-3000,6000), ylim=c(-500,6000),
            grid=TRUE,
            xlab="Extraordinary", ylab="Holdover",
            main="Extraordinary vs Holdover", col=3)



############ FORMA 2: 

## função: dataEllipse{cars}  e outro banco de dados


### Livro do Johnson e Wichern, 6a edição:
### Pag: 445, Exemplo 8.4 - T2 de Hotelling, Capítulo 8.
### Dados Tabela 6.9
## desloquei os valores dos MALES como Width[25:48]+40

dados2=data.frame(read.csv(file.choose(), header=TRUE, sep=";"))
attach(dados2)

W=c(Width[1:24],Width[25:48]+40)  ## desloquei os valores dos MALES
par(mfrow=c(1,1))
x11();with(dados2, dataEllipse(Length, W, factor(Sexo),
                              pch=c(19,19),
                              xlim=c(70, 200), center.pch="+",
                              group.labels=c("female", "male"),
                              ylim=c(60, 150), 
                              level=.95, fill=TRUE, fill.alpha=0.1))



##################### Via ggplot2 - código do Victor Navarro

############ FORMA 3: 


require(ggplot2)

names(dados2)
medias=data.frame(colMeans(dados2[,1:2]))
confianca=0.90

par(mfrow=c(1,1))
x11()
ggplot(dados2, aes(x=Length, y=Width) )+
  geom_point() + 
  stat_ellipse(level=confianca, type="t") +
  stat_ellipse(level=confianca+0.05, type="t", colour="red")+
  stat_ellipse(level=confianca+0.09, type="t", colour="blue")+
  geom_hline(yintercept=medias[2,1])+
  geom_vline(xintercept=medias[1,1])
