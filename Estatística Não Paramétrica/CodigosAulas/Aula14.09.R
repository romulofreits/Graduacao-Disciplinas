?shapiro.test
set.seed(123456)
n <- 100
x <- rnorm(n = n, mean = 10)
shapiro.test(x)

n <- 100
mu <- 10
sigma <- 2
set.seed(12345678)
x <- rnorm(n, mean = mu, sd = sigma)
qqnorm(x)
abline(a = mu, b = sigma, col = 2)


#SHAPIRO FRANCIA: ANALISA O QQPLOT
M <- 1e3
n <- 100
plot(0, 0, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), type = "n",
     xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
     main = "Bandas de Confiança para o QQ-plot")
x <- matrix(rnorm(M * n), nrow = n, ncol = M)
matpoints(qnorm(ppoints(n)), apply(x, 2, sort), pch = 19, cex = 0.5,
          col = gray(0, alpha = 0.01))
abline(a = 0, b = 1)
p <- seq(0, 1, l = 1e4)
xi <- qnorm(p)
lines(xi, xi - qnorm(0.975)/sqrt(n) * sqrt(p * (1 - p))/dnorm(xi),
      col = 2, lwd = 2)
lines(xi, xi + qnorm(0.975)/sqrt(n) * sqrt(p * (1 - p))/ dnorm(xi),
      col = 2, lwd = 2)

# Nao rejeita H0
install.packages('nortest')
library(nortest)

set.seed(123456)
n <- 100
x <- rnorm(n = n, mean = 10)
nortest::sf.test(x)
# QUANDO AUMENTAMOS OS GRAUS DE LIBERDADE DA DISTRIBUICAO T-STUDENT, ELA SE APROXIMA DE UMA DISTRIBUICAO NORMAL

# Rejeita H0
x <- rt(n = n, df = 3)
nortest::sf.test(x)

# Estatistica de teste
cor(x = sort(x), y = qnorm(ppoints(n, a = 3/8)))^2

# CRAMER VON-MISES
install.packages('goftest')
library(goftest)
?goftest::cvm.test

# Amostra N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)
goftest::cvm.test(x = x, null = "pnorm", mean = 10, sd = 4) #padrao: N(0,1)

ks.test(x = x, y = "pnorm")

# gerar uma weibull
# nao sabemos os parametros, logos utilizamos a versao passando os parametros
set.seed(521353)
x = rweibull(100, shape = 2, scale = 5)
goftest::cvm.test(x, 'pweibull', shape = 2, scale = 5)

# se atentar a parametrizacao padrao do R
?rweibull
?rexp

ks.test(x = x, y = "pnorm")

# ANDERSON DARLING
# SE NAO CONHECEMOS OS PARAMETROS, COLOCAMOS A VERSAO ETIMADA DOS PARAMETROS
?goftest::ad.test
# Amostra de uma N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)
goftest::ad.test(x = x, null = "pnorm")

# weibull geometrica no R
install.packages('elfDistr')
library(elfDistr)
?`elfDistr-package`

# Estimar os parametros de uma distribuicao no R
install.packages('mle')
library(mle)

# FUNCAO PARA ESTIMAR OS PARAMETROS DE UMA DISTRIBUICAO
nll = function(shape, scale) - sum(dweibull(y, shape, scale, log = TRUE))
fit0 = stats4::mle(nll, start = list(shape = 1, scale = 1), nobs = length(dados))

dados = scan() # ler os dados de forma automatica
# Quais as estimativas de maxima verossimilhanca que esta no artigo?
# https://www.rdocumentation.org/packages/stats4/versions/3.6.2/topics/mle
# https://listas.inf.ufpr.br/pipermail/r-br/2014-October/014457.html
# https://cran.r-project.org/web/packages/e1071/e1071.pdf
# https://cran.r-project.org/web/packages/elfDistr/elfDistr.pdf

#install.packages('readr')
library(readr)
#install.packages('e1071')
library(e1071)
#install.packages('dplyr')
library(dplyr)

install.packages('readr')
library(readr)
dados.equipe1 = read_csv('Dados_Equipe_n1.txt', col.names = FALSE)
dados.equipe1 %>% summarise(
  n = length(dados),
  media = mean(dados),
  sd = sd(dados),
  q1 = quantile (dados, probs = 0.25),
  q2 = quantile (dados, probs = 0.5),
  q3 = quantile (dados, probs = 0.75),
  sss = skewness(dados),
  curt = kurtosis(dados)
)


