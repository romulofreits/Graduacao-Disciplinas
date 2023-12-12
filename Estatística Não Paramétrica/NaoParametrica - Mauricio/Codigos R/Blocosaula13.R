

Y=c(44,59,60,41,33,19,49,71,44,40,45,31,68,66,70,64,63,49,59,71,64,73,77,67)/10
n=length(Y);n

ex02 <- data.frame(estag = factor(rep(1:6, each=4)), bloco=factor(rep(1:4, 6)),
 resp=Y)

ex02

names(ex02)
summary(ex02)
attach(ex02)
plot(resp ~ estag + bloco)
interaction.plot(estag, bloco, resp)
interaction.plot(bloco, estag, resp)

ex02.mt <- tapply(resp, estag, mean)

ex02.mt

ex02.mb <- tapply(resp, bloco, mean)

ex02.mb
ex02.var <- tapply(resp, bloco, var)

ex02.var
ex02.vart <- tapply(resp, estag, var)

ex02.vart

plot.default(estag, resp)

points(ex02.mt, pch="x", col=2, cex=1.5)

plot.default(bloco, resp)

points(ex02.mb, pch="x", col=2, cex=1.5)

ex02.av <- aov(resp ~ bloco + estag)
vcov(ex02.av)
round(vcov(ex02.av),2)
anova(ex02.av)
summary(ex02.av)

names(ex02.av)
ex02.av$model
ex02.av$coefficients
model.matrix(ex02.av) #obter a matrix de experimentos
par(mfrow=c(2,2))

plot(ex02.av)

par(mfrow=c(2,1))
residuos <- (ex02.av$residuals)

plot(ex02$bloco,residuos)

title("Res´ıduos vs Blocos")

plot(ex02$estag,residuos)
title("Res´ıduos vs Est´agios")
par(mfrow=c(2,2))
preditos <- (ex02.av$fitted.values)
plot(residuos,preditos)
title("Res´ıduos vs Preditos")
respad <- (residuos/sqrt(anova(ex02.av)$"Mean Sq"[2]))
boxplot(respad)
title("Res´ıduos Padronizados")
qqnorm(residuos,ylab="Residuos", main=NULL)
qqline(residuos)
title("Grafico Normal de \n Probabilidade dos Res´ıduos")
## teste para normalidade

shapiro.test(residuos)


## Testando a não aditividade

## primeiro vamos extrair coeficientes de tratamentos e blocos
ex02.av$coeff
bl <- c(0, ex02.av$coeff[2:4])
tr <- c(0, ex02.av$coeff[5:9])

bl
tr
## agora criar um novo termo e testar sua significancia na ANOVA
bltr <- rep(bl, 6) * rep(tr, rep(4,6))
ttna <- update(ex02.av, .~. + bltr)
anova(ttna)


ex02.tk <- TukeyHSD(ex02.av, "estag", ord=T)
ex02.tk
plot(ex02.tk)
detach(ex02)
matrix(ex02.av)
X=model.matrix(ex02.av)
X
X[[1]]
names(X)



