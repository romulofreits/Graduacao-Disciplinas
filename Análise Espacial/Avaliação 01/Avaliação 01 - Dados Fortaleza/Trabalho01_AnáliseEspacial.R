# Carregamento dos pacotes
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr)
install.packages('hrbrthemes')
library(hrbrthemes)
install.packages('esquisse')
library(esquisse)
install.packages('ggrastr')
library(ggrastr)
library(ggplot2)
install.packages('tidyverse')
library(tydiverse)

# Carregamento do banco de dados
dados = read.csv('Bairros_Fortaleza.csv')
View(dados)
glimpse(dados) # função para identificar o tipo das variáveis
head(dados)

# Medidas descritivas do IDHB 
summary(dados$IDHB2010)
sd(dados$IDHB2010)
summary(dados$IDHB2010Educacao)
round(sd(dados$IDHB2010Educacao), 4)
summary(dados$IDHB2010Renda)
round(sd(dados$IDHB2010Renda), 5)

summary(dados$Populacao_Total)
# Medidas descritivas dos nao alfabetizados e dos alfabetizados
summary(dados$RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS.)
summary(dados$RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.)
round(sd(dados$RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS.), 4)
round(sd(dados$RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.), 4)


# Medidas descritivas da renda por cor
summary(dados$Numero_de_Cor.Raca_Amarela)
summary(dados$Numero_de_Cor.Raca_Branca)
summary(dados$Numero_de_Cor.Raca_Indigena)
summary(dados$Numero_de_Cor.Raca_Parda)
summary(dados$Numero_de_Cor.Raca_Preta)

plot(dados$IDHB2010, dados$RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS.)
plot(dados$IDHB2010, dados$RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.)

ggplot(dados, aes(x=IDHB2010,
                  y=RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS.,
                  color=IDHB2010)) + 
  geom_point(size=6) +
  theme_ipsum()

par(mfrow = c(1,2))

# Correlacao entre o IDHB e a renda mensal dos alfabetizados
p1 = ggplot(dados, aes(y=IDHB2010, 
                        x=RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS.)) +
  geom_point(size = 3,pch = 21, color = 'firebrick4', fill = 'firebrick1')+ 
  labs(title = 'Correlação entre o IDHB - 2010 e a renda dos alfabetizados', x = 'Renda média mensal dos alfabetizados', y = 'IDHB2010')

p1

p2 = ggplot(dados, aes(y=IDHB2010, 
                       x=RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.)) +
  geom_point(size = 3,pch = 21, color = 'firebrick4', fill = 'firebrick1')+ 
  labs(title = 'Correlação entre o IDHB - 2010 e a renda dos não alfabetizados', x = 'Renda média mensal dos não alfabetizados', y = 'IDHB2010')

p2
###############################################################
p2 = ggplot(dados, aes(y=IDHB2010, 
                       x=dados$RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.)) +
  geom_point(size = 3.5, color = 'cyan4') +
  geom_smooth(method = lm , color="red", se=FALSE, na.rm = F) +
  theme_ipsum()+
  labs(title = 'Correlação entre o IDHB - 2010 e a renda dos não alfabetizados', x = 'Renda média mensal dos não alfabetizados', y = 'IDHB2010')

p2
?title
# Correlacao entre o IDHB e a renda mensal dos nao alfabetizados
ggplot(dados, aes(y=IDHB2010, 
                        x=RENDA_MEDIA_MENSAL_NAO_ALFABETIZADOS_.RS.)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum_es() +
  labs(y = 'Renda média mensal da população não alfabetizada')

p2
#**********************************************************#
mod1 = lm(Y~X, dados$RENDA_MEDIA_MENSAL_ALFABETIZADOS_.RS. ~ dados$IDHB2010);mod1

summary(mod1)
#**********************************************************#

# Usando o pacote esquisse para gerar gráficos
esquisser(dados)

# Boxplot

ggplot(dados, aes(y = IDHB2010, x = '')) +
  geom_errorbar(stat = 'boxplot', width = 0.2) +
  geom_boxplot_jitter(width = 0.6, fill = 'grey90', outlier.shape = 1, outlier.size = 2, outlier.jitter.height = 0) +
  geom_point(stat = 'summary', fun = 'mean', shape = 4, size = 2.5, color = 'firebrick3') +
  #coord_flip() +
  labs(x = 'Renda Média Mensal', y = 'IDHB2010')+
  theme_classic()
  
ggpairs(dados, columns = 2:5)
