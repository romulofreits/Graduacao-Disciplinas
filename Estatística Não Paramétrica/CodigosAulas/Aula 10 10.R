n <- 20
v <- 0:(0.5*n*(n+1))
dsignrank(v, n)

par(mar = c(4.0,4.0,0.1,0.1))
plot(v, dsignrank(v, n), type = "h",
     ylab = "Pr(V = v)" )

#Dados
#m0 = 175
#H1 unilateral a direita
x <- c(254, 171, 345, 134,
       190, 447, 106, 173, 449, 198)

m0 <- 175

n <- length(x)
v <- 0:(0.5*n*(n+1))


z <- x - m0; z
r <- rank(abs(z)); r
r_sinalizado <- ifelse(z>0, r, -r); r_sinalizado
V <- sum(r[z>0]); V

#valor-p
#P(V >= v_obs)
#opcao 1
sum(dsignrank(V:(0.5*n*(n+1)), n))
#opcao 2
psignrank(V-1, n, lower.tail = FALSE)
#opcao 3
wilcox.test(x, mu = 175, alternative = "greater")

par(mar = c(4.0,4.0,0.1,0.1))
plot(v, dsignrank(v, n), type = "h",
     ylab = "Pr(V = v)" )
points(V:(0.5*n*(n+1)),
       dsignrank(V:(0.5*n*(n+1)), n),
       type = "h",
       col = 2)


#sem correcao de continuidade
wilcox.test(x, mu = 175, alternative = "greater",
            exact = FALSE, correct = FALSE)

esp_V <- (n*(n+1))/4
var_V <- (n*(n+1)*(2*n + 1))/24

Z_v <- (V - esp_V)/sqrt(var_V)
pnorm(Z_v, lower.tail = FALSE)

#com correcao de continuidade
wilcox.test(x, mu = 175, alternative = "greater",
            exact = FALSE)

Z_v <- (V - esp_V-0.5)/sqrt(var_V)
pnorm(Z_v, lower.tail = FALSE)

wilcox.test(x, mu = 175, alternative = "greater",
            conf.int = TRUE)
