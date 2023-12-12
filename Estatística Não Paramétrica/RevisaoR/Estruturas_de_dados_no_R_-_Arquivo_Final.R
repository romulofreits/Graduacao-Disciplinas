#---------------------------------- LISTAS ----------------------------------#
# Assim como em Python, as listas em R são estruturas de dados ordenadas e
# mutáveis que armazenam variáveis de diferentes tipos e tamanhos

# Criando uma lista com os valores 'Lucas','Jean' e 'Lira'
lista <- list('Lucas','Jean','Lira')

# Verificando o comprimento dessa lista
length(lista)

# Acessando o segundo elemento da lista
lista[2]

# Alterando o segundo elemento para Renato
lista[2] <- 'Renato'

# Retornando a lista sem o Renato
lista <- lista[-2]

# Adicionando o elemento Leo no fim da lista
lista <- append(lista,'Leo')

# Agora adicionando Glaucia após o Lira
lista <- append(lista,'Glaucia',after=2)

lista <- append(lista,10)

#--------------------------------- VETORES ---------------------------------#
# Vetor é uma estrutura unidimensional (1D), com todos os dados do mesmo tipo

# Criando um vetor com os mesmos valores da lista
vetor <- c('Lucas','Jean','Lira')

# Criando um novo vetor com as idades 30, 30 e 28
idades <- c(30,30,28)

# Alterando o primeiro valor do vetor das idades para "Lucas"
idades[1] <- 'Lucas'

# Acessando o elemento na posição 2 dessa lista
idades[2]

# Sendo o vetor mostrado abaixo
a <- c('Lucas','Jean','Lira','Pedro','Glaucia','Renato','Bia','Alon')
# Vamos acessar apenas os valores Lucas, Glaucia e Alon
b <- c(1,5,8)
a[b]

# Criando uma sequência de 1 a 10
seq1_10 <- 1:10

# Criando essa mesma sequência apenas com valores ímpares
seq_impares = seq(1,10,2)

# Verificando o máximo dessa sequência
max(seq1_10)

# A média
mean(seq1_10)

# E o mínimo
min(seq1_10)

#--------------------------------- MATRIZES ---------------------------------#
# Matrizes são estruturas de dados bidimensionais (2D), também com todos
# os dados do mesmo tipo

# Criando uma matriz com os valores de 1 a 6 com 2 linhas e 3 colunas
matriz <- matrix(1:6,2,3)

# Acessando o valor 5 (primeira linha, terceira coluna)
matriz[1,3]

# Acessando somente a primeira linha
matriz[1,]

matriz[1,3] <- 'Lucas'

#-------------------------------- DATAFRAMES --------------------------------#
# Data Frames são estruturas bidimensionais assim como as matrizes, porém 
# permite que cada uma das colunas possua tipos diferentes de dados

# Sendo as colunas abaixo
nomes <- c('Lucas','Jean','Lira','Alon') 
idades <- c(30,30,28,26)
notas <- c(4.5,7.6,9.3,9)

# Criando o Data Frame
dataframe <- data.frame(nomes,idades,notas)

# Visualizando as dimensões
dim(dataframe)

# Número de linhas
nrow(dataframe)

# Número de colunas
ncol(dataframe)

# Acessando a segunda coluna
dataframe[2]
dataframe[['idades']]
dataframe$idades

# Visualizando um resumo do DataFrame
summary(dataframe)