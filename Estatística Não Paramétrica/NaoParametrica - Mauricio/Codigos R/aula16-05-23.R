# Exemplo 3 pg 517 (Mood)
n = 5
k = 5
j =1
beta = 0.75
pbeta(beta, k-j, n-k+j+1, lower.tail=F)

# Fazendo pela binomial
pbinom(n, n, beta) - pbinom(k-j-1, n, beta)

p1=pbeta(0.63, 5, 10); p1
# p2 = P(X >= k) = 1 - P(X <= k-1)
p2=1-pbinom(4, 14, 0.63); p2
