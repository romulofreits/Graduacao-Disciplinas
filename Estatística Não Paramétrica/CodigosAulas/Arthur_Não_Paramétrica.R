15*1.2
dpois(0,1.2)*60
15/60

v = c(15,25,10,5,4,1)
plot(ecdf(v))

acf_te = c(ppois(0,1.2))
for(i in 1:5){
  acf_te[i+1] = ppois(i,1.2)
}

acf_em = c(15/60)
for(i in 1:5){
  acf_em[i+1] = acf_em[i]+(v[i+1]/60)
}

round(acf_te,2)
round(acf_em,2)

round(ppois(0:5,1.2),2)

?ks.test(v)
ks.test(v,acf_te)

sumario = cbind(c(0:5),v,acf_em,acf_te); round(sumario,2)
diff = abs(acf_te - acf_em); round(diff,2)
diff2 = c(0)
for(i in 1:5){
  diff2[i+1] = abs(acf_te[i+1]-acf_em[i])
}
round(diff2,2)
sumario = cbind(sumario,diff,diff2); round(sumario,2)
sumario = round(sumario,2); sumario
class(sumario)

sumario = as.data.frame(sumario); class(sumario); names(sumario)
colnames(sumario) = c('X', 'fi', 'Fn', 'Fx', '|Fx-Fn|', '|Fx(i+1) - Fn(i-1)|')
sumario
