#----------------- Regressao Linear Multipla -----------------------------#

# Passo 1: Carregar os pacotes que serao usados
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

setwd("C:/Users/romul/OneDrive/Área de Trabalho/Semestre 2023.2/Modelos de Regressão I/RegressaoLinearMultipla/CodigosR")

dados = read.csv2('Banco de Dados 12.csv')
View(dados) # visualizacao dos dados em janela separada
glimpse(dados) # visualizacao de um resumo dos dados

# Construcao do modelo:
# Os pressupostos nao sao emcima da variavel dependente, mas sim emcima do residuo do modelo.
mod = lm(Notas ~ Tempo_Rev + Tempo_Sono, dados)

# modelo com interacao
# mod = lm(Notas ~ Tempo_Rev + Tempo_Sono + Tempo_Sono*Tempo_Rev, dados)

# Analise grafica:
par(mfrow=c(2,2)) # todos os graficos devem sair em duas linhas e duas colunas
plot(mod)
par(mfrow=c(1,1))

# Teste de Normalidade dos residuos:
shapiro.test(mod$residuals)
library(nortest)
lillie.test(mod$residuals)

# Outliers nos residuos:
summary(rstandard(mod)) # analisando os residuos padronizados
# esperamos que nao haja nenhum valor fora do intervalo -3 e 3

# Independencia dos residuos (Durbin-Watson):
durbinWatsonTest(mod)

# Homocedasticidade (Breusch-Pagan):
bptest(mod)

# Ausencia de Multicolinearidade:
pairs.panels(dados, hist.col = 'steelblue')
### Multicolinearidade: r > 0.9 (ou 0.8)

vif(mod)
### Multicolinearidade: VIF > 10

# Criacao de um segundo modelo
mod2 = lm(Notas ~ Tempo_Rev, dados)

# Passo 4: Analise do modelo
summary(mod)
summary(mod2)

# Obtencao dos coeficientes padronizados
lm.beta(mod)
lm.beta(mod2)

## Obtencao do IC 95% para os coeficientes
confint(mod)
confint(mod2)

# Comparacao de modelos

## AIC e BIC - Comparacao entre quaisquer modelos
AIC(mod, mod2)
BIC(mod, mod2)

# Para comparacao entre modelos aninhados
anova(mod, mod2)
# O melhor sera o com menor valor de RSS (residual sum of squares)

# Passo 5: Grafico de dispersao
graph = scatterplot3d(dados$Notas ~ dados$Tempo_Rev + dados$Tempo_Sono,
                       pch = 16, angle = 30, color = "steelblue", box = FALSE,
                       xlab="Tempo de revisão", ylab="Tempo de sono", zlab="Notas")
graph$plane3d(mod, col="black", draw_polygon = TRUE)

#----------------- Metodos de Selecao de Modelos -----------------------#
pacman::p_load(MASS)
mod.inicial = lm(Notas ~ Tempo_Rev + Tempo_Sono, data = dados)
mod.simples = lm(Notas ~ 1, data = dados)

stepAIC(mod.inicial, scope = list(upper = mod.inicial,
                                  lower = mod.simples), direction = "backward")

# A regressao linear multipla mostrou que o tempo de revisao e o tempo de sono tem influencia sobre as notas. A cada 1 minuto gasto revisando o conteudo, a nota aumenta em media 0,10 (t = 10,005; p < 0,001). Ja a cada hora de sono, a nota aumenta, em media, 0,35 (t = 4,026; p < 0,001).






