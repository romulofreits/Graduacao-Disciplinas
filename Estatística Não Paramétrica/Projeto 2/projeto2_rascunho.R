setwd("C:/Disciplinas Graduacao/Estatística Não Paramétrica/Projeto 2")

library(tidyr)
library(tidyverse)
library(e1071)
library(xtable)
library(ggplot2)
library(ggpubr)
library(nortest)
library(DescTools)
library(fitdistrplus)

dados = read.csv('dados_projeto.csv')
colnames(dados) = c('jogador', 'prop_vitorias')
dados['vitorias'] = round(dados$prop_vitorias*112)
View(dados)

medidas.descritivas_prop = dados %>%
  summarise(
    media = mean(dados$prop_vitorias),
    q1 = quantile(dados$prop_vitorias, probs = 0.25),
    q2 = median(dados$prop_vitorias),
    q3 = quantile(dados$prop_vitorias, probs = 0.75),
    desv.pd = sd(dados$prop_vitorias),
    minimo = min(dados$prop_vitorias),
    maximo = max(dados$prop_vitorias),
    curt = kurtosis(dados$prop_vitorias),
    assi = skewness(dados$prop_vitorias)
  );medidas.descritivas_prop

medidas.descritivas_vit = dados %>%
  summarise(
    media = mean(dados$vitorias),
    q1 = quantile(dados$vitorias, probs = 0.25),
    q2 = median(dados$vitorias),
    q3 = quantile(dados$vitorias, probs = 0.75),
    desv.pd = sd(dados$vitorias),
    minimo = min(dados$vitorias),
    maximo = max(dados$vitorias),
    curt = kurtosis(dados$vitorias),
    assi = skewness(dados$vitorias)
  );medidas.descritivas_vit

# Crie uma tabela LaTeX a partir das medidas descritivas
tabela_latex <- xtable(medidas.descritivas)

# Imprima a tabela LaTeX
print(tabela_latex, type = "latex")

tabela_latex2 <- xtable(medidas.descritivas2)
print(tabela_latex2, type = "latex")

tabela_latex3 = xtable(dados)
print(tabela_latex3)
#=============== ANALISE GRAFICA DE NORMALIDADE ==========================#
# Proporcao de vitorias
par(mfrow=c(2,2))
boxplot(dados$prop_vitorias, col = 'white', 
        main = 'Boxplot da Proporção de Vitórias')
denscomp(list(fit_n), ylab = 'densidade', 
         xlab = 'dados', main = 'Histograma da Proporção de Vitórias', 
         xlegend = NULL, ylegend = NULL)

qqnorm(dados$prop_vitorias, ylab = 'quantis amostrais', xlab = 'quantis teóricos')
qqline(dados$prop_vitorias)
cdfcomp(list(fit_n), legendtext = plot.legend, 
        main = 'FDA Empírica e FDA Teórica', ylab = 'FDA', xlab = 'dados')

shapiro.test(dados$prop_vitorias)
ks.test(dados$prop_vitorias, 'pnorm', mean = 0.41333617, sd = 0.09864619)

# numero de vitorias
par(mfrow=c(2,2))
boxplot(dados$vitorias, col = 'white', 
        main = 'Boxplot do Número de Vitórias')
denscomp(list(fit_n2), ylab = 'densidade', 
         xlab = 'dados', main = 'Histograma do Número de Vitórias', 
         xlegend = NULL, ylegend = NULL)

qqnorm(dados$vitorias, ylab = 'quantis amostrais', xlab = 'quantis teóricos')
qqline(dados$vitorias)
cdfcomp(list(fit_n2), legendtext = plot.legend, 
        main = 'FDA Empírica e FDA Teórica', ylab = 'FDA', xlab = 'dados')

shapiro.test(dados$vitorias)
ks.test(dados$vitorias, 'pnorm', mean = 46.23810, sd = 11.16075)

#===========================================================================#
# Teste Binomial:
p_valores = c()
n = length(dados$vitorias)
ic_inf = c()
ic_sup = c()

for (i in 1:n) {
  teste = binom.test(dados$vitorias[i], 112, alternative = 'two.sided',
                     conf.level = 0.95)
  p_valores[i] = teste$p.value
  ic_inf[i] = teste$conf.int[1]
  ic_sup[i] = teste$conf.int[2]
}

dados2 = data.frame(Jogador = 1:n, P_Valor = round(p_valores, 4), 
                             IC_Inferior = round(ic_inf, 4), IC_Superior = round(ic_sup, 4))

View(dados2)

xtable(dados2, auto = T)



#------------------------------------------------------------------------#

tabela = data.frame(tabela)
View(tabela)
colnames(tabela) = c('di', 'di-0.5', '|di-0.5|', 'rank(di)')

SignTest(dados$vitorias, mu = 0.5, alternative = 'two.sided')

for (i in dados$vitorias) {
  teste = prop.test(i, 112, alternative = 'two.sided',
                    conf.level = 0.95, correct = TRUE)
  cat("jogador", ":", teste$p.value, "\n")
}

#===========================================================================#
prop_vit = dados$prop_vitorias
a = ifelse(prop_vit >= 0.5, 1, 0)
sum(a == 1)
binom.test(sum(a == 1), length(a), 0.5, alternative = 'two.sided')






# vetor que retorna 0 ou 1
gerar_vetor_binario = function(vetor) {
  resultado = ifelse(vetor >= 0.5, 1, 0)
  return(resultado)
}

wilcox.test(dados$prop_vitorias, mu = 0.5, alternative = 'less',
            exact = FALSE, correct = FALSE, conf.int = TRUE)

wilcox.test(dados$prop_vitorias, mu = 0.5, alternative = 'greater',
            exact = FALSE, correct = FALSE, conf.int = TRUE)

wilcox.test(dados$prop_vitorias, mu = 0.5, alternative = "two.sided",
            exact = TRUE, correct = FALSE, conf.int = TRUE)

t.test(dados$prop_vitorias, mu = 0.5, alternative = 'greater', 
       conf.level = 0.95)

#==========================================================================#
wal = function (x) 
{
  n <- length(x)
  w <- vector(n * (n + 1)/2, mode = "numeric")
  ind <- 0
  for (i in 1:n) {
    for (j in i:n) {
      ind <- ind + 1
      w[ind] <- 0.5 * (x[i] + x[j])
    }
  }
  return(w)
}

wal(dados$prop_vitorias)
wal(dados$vitorias)
HodgesLehmann(dados$prop_vitorias)
#=========================================================================#

#GRAFICO PARA OS ICS
ggplot(data = dados2, aes(x = Jogador, ymin = IC_Inferior, ymax = IC_Superior, 
                          color = (IC_Inferior >= 0.5 | IC_Superior >= 0.5))) +
  geom_linerange(size = 1.5) +  # Aumenta a espessura das linhas
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(x = "Jogador", y = "Intervalo de Confiança") +
  theme_bw() +  # Fundo branco
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +  # Remove as linhas de grade
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  guides(color = FALSE)  # Remove a legenda de cores






