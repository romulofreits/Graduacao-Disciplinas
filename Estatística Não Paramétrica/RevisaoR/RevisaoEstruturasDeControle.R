## Estruturas de controle

maior_fun = function(a, b){
  if(a == b){
    return(c("Valores iguais"))
  }else{
    if(a > b){
      maior = a
    }else{
      maior = b
    }
    return(cat("Maior:", maior)) #cat: faz com que o valor retornado seja um numero
  }
  return(cat("Maior:", maior))
}

maior_fun(2,6)

## Exemplo 2

Hollerich = function(salarioBruto, premio, baseINPS, baseIR, taxaINPS, taxaIR){
  INPS = 0
  IR = 0
  Rendimentos = salarioBruto + premio
  if(Rendimentos > baseINPS) INPS = taxaINPS*Rendimentos
  if((Rendimentos - INPS) > baseIR) IR = taxaIR*(Rendimentos-INPS)
  salarioLiquido = Rendimentos - INPS - IR
  list(salarioBruto = salarioBruto, premio = premio,
       Rendimento = Rendimentos, INPS = INPS, IR = IR, salarioLiquido = salarioLiquido)
}

Hollerich(45000, 10000, 30000, 50000, 0.10, 0.15)

#--------------------Outra Maneira-------------------------------------#
Hollerich2 = function(salarioBruto, premio, baseINPS, baseIR, taxaINPS, taxaIR){
  #INPS = 0
  #IR = 0
  
  Rendimentos = salarioBruto + premio
  
  ifelse(Rendimentos > baseINPS, INPS = taxaINPS*Rendimentos, 0)
  
  ifelse((Rendimentos - INPS) > baseIR, IR = taxaIR*(Rendimentos-INPS), 0)
  
  salarioLiquido = Rendimentos - INPS - IR
  list(salarioBruto = salarioBruto, premio = premio,
       Rendimento = Rendimentos, INPS = INPS, IR = IR, salarioLiquido = salarioLiquido)
}

Hollerich2(45000, 10000, 30000, 50000, 0.10, 0.15)







