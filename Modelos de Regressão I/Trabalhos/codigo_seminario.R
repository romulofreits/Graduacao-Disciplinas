library(tidyverse)
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

setwd("C:/Users/romul/OneDrive/Área de Trabalho/Semestre 2023.2/Modelos de Regressão I/RegressaoLinearMultipla/Exercicios/Cap11")

df = read.table('Ex11_04.txt', header = TRUE)
names(df) = c('times','y', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9')
df = df[-1]
# View(df)
# glimpse(df)


# pairs.panels(df[-1])

full_m = lm(y~., df);summary(full_m) # modelo completo
int_m = lm(y~1, df); summary(int_m) # modelo apenas com intercepto

# forward
f = step(int_m, direction = "forward", scope = formula(full_m))

# back
b = step(full_m, direction = "backward", scope = formula(full_m))

# stepwise
stw = step(full_m, direction = "both", scope = formula(full_m))

#============================================================================
library(olsrr)

# stepwise forward regression:
ols_step_forward_p(full_m, penter = 0.05, details = TRUE)
ols_step_both_aic(full_m, details = TRUE)
ols_step_both_p(full_m, penter = 0.05, details = TRUE)
ols_step_forward_aic(full_m, details = TRUE)
ols_step_backward_aic(full_m, details = TRUE)
ols_step_backward_p(full_m, details = T, penter = 0.05)
#ols_step_all_possible(full_m, details = T)
ols_leverage(full_m)
ols_norm_test(full_m)
ols_step_best_subset(full_m, details = T)
#ols_step_forward(full_m, details = TRUE)

plot(ols_step_forward_p(full_m, penter = 0.05, details = TRUE))
plot(ols_step_forward_aic(full_m, details = TRUE))

#==========================================================================
# Modelos Selecionados:
m1 = lm(y ~ x2+x4+x7+x8+x9, df)
m2 = lm(y ~ x2+x7+x8+x9, df)
m3 = lm(y ~ x2+x7+x8, df)
#=============================================================================

vif(mod) # multicolinearidade
bptest(mod) # Homocedasticidade (Breusch-Pagan)
durbinWatsonTest(mod) # Independencia dos residuos (Durbin-Watson):
summary(rstandard(mod))  # Outliers nos residuos:
shapiro.test(mod$residuals) # Normalidade dos residuos

# matriz de correlacao:
library(GGally)
df_novo = df[c('x2', 'x4', 'x7', 'x8', 'x9')]
ggpairs(df_novo)
