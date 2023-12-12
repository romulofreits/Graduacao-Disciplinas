<<<<<<< HEAD

### T2 de Hotelling para 2 grupos NÃO PAREADOS e elipsóides:


library(MASS)
library(DescTools)

math.teach <- data.frame(
  teacher = factor(rep(1:2, c(3, 6))),
  satis   = c(1, 3, 2, 4, 6, 6, 5, 5, 4),
  know    = c(3, 7, 2, 6, 8, 8, 10, 10, 6))
math.teach
 
HotellingsT2Test(cbind(math.teach$satis, math.teach$know) ~ math.teach$teacher, test = "f")


## Exemplo dados Lizard, exemplo 6.17 - Johnson e Wichern

dados=data.frame(lagartos); dados
attach(dados)

plot(log(Peso), log(Focinho), 
     col=factor(Genero), pch=19)

HotellingsT2Test(cbind(log(Peso), log(Focinho)) ~ dados$Genero, 
                 test = "f")


require(car)
dataEllipse(log(Peso), log(Focinho), factor(Genero),
            pch=c(19,19), center.pch="+",
            xlim=c(0.5,4.5), ylim=c(3.7,5),
            group.labels=c("C", "S"),
            level=.95, fill=TRUE, fill.alpha=0.1,
            main="Dados lizard, exemplo 6.17 - Johnson e Wichern")


=======

### T2 de Hotelling para 2 grupos NÃO PAREADOS e elipsóides:


library(MASS)
library(DescTools)

math.teach <- data.frame(
  teacher = factor(rep(1:2, c(3, 6))),
  satis   = c(1, 3, 2, 4, 6, 6, 5, 5, 4),
  know    = c(3, 7, 2, 6, 8, 8, 10, 10, 6))
math.teach
 
HotellingsT2Test(cbind(math.teach$satis, math.teach$know) ~ math.teach$teacher, test = "f")


## Exemplo dados Lizard, exemplo 6.17 - Johnson e Wichern

dados=data.frame(lagartos); dados
attach(dados)

plot(log(Peso), log(Focinho), 
     col=factor(Genero), pch=19)

HotellingsT2Test(cbind(log(Peso), log(Focinho)) ~ dados$Genero, 
                 test = "f")


require(car)
dataEllipse(log(Peso), log(Focinho), factor(Genero),
            pch=c(19,19), center.pch="+",
            xlim=c(0.5,4.5), ylim=c(3.7,5),
            group.labels=c("C", "S"),
            level=.95, fill=TRUE, fill.alpha=0.1,
            main="Dados lizard, exemplo 6.17 - Johnson e Wichern")


>>>>>>> 785f9abbd9b17f1cfe481092cee34c288e1574e6
