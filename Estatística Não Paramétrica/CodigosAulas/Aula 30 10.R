# Estatistica Nao Parametrica 30/10/23

library(rio)
library(ggpubr)
library(tidyverse)

seed <- import(file = "http://lea.estatistica.ccet.ufrn.br/tutoriais/dados/seed.xls")
dados_seed <-  dados <- data.frame(Semente = c(rep("Normal", 11), 
                                               rep("Seco",11)),
                                   Rendimento = c(seed$`Regular seed`,
                                                  seed$`Kiln-dried seed`))

ggboxplot(dados_seed,
          x = "Semente",
          y = "Rendimento",
          xlab = "Tipo de Semente",
          ylab = "Rendimento",
          fill = "Semente",
          legend = 'none',
          ggtheme = theme_bw()
) + stat_compare_means(paired = TRUE,
                       vjust = -0.8,
                       label.sep = ": ",
                       method.args = list(alternative = "greater",
                                          exact = TRUE))


twins <- import(file = "http://lea.estatistica.ccet.ufrn.br/tutoriais/dados/twins.xls")
Twins <-
  pivot_longer(twins, cols = c(`Twin 1`, `Twin 2`)) |>
  rename(Twin = name, TesteQi = value) |>
  mutate(Gemeos = if_else(Twin == "Twin 1", "Gemeo 1", "Gemeo 2"))


ggboxplot(Twins,
          x = "Gemeos",
          y = "TesteQi",
          xlab = "Gêmeo",
          ylab = "Teste de QI",
          fill = "Gemeos",
          legend = 'none',
          ggtheme = theme_bw()
) + stat_compare_means(paired = TRUE,
                       vjust = -0.8,
                       label.sep = ": ",
                       method.args = list(alternative = "greater"))
