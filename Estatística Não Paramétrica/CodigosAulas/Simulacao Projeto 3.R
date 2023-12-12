poder.t = c()
poder.wilcox = c()
set.seed(19)

poder = function(n) {
  poder.t = mean(replicate(5000, t.test(rnorm(n, 5, 2), mu = 4)$p.value < 0.05))
  poder.wilcox = mean(replicate(5000, wilcox.test(rnorm(n, 5, 2), mu = 4)$p.value < 0.05))
  return(list(poder.t = poder.t, poder.wilcox = poder.wilcox))
}

n = c(10, 25, 40, 55, 70)
for (i in 1:length(n)) {
  poderes = poder(n[i])
  poder.t[i] = poderes$poder.t
  poder.wilcox[i] = poderes$poder.wilcox
  cat("Tamanho da amostra:", n[i], "\n")
  cat("Poder teste t:", poder.t[i], "\n")
  cat("Poder teste Wilcoxon:", poder.wilcox[i], "\n\n")
}
