-- Consultas do banco de dados lojas:

-- Listar o nome e o sal�rio de todos os vendedores lotados na filial 01. 
-- No resultado, os nomes das colunas dvem ser Nome do Vendedor e Sal�rio L�quido
select nome as 'Nome do Vendedor', salario as 'Salario L�quido'
from vendedores
where codfil = 01

-- Listar o c�digo dos itens que tiveram vendas
select distinct coditem
from historico

-- Listar todos os vendedores com sobrenome 'brayner'
select nome from vendedores
where nome like '%brayner%'

-- JUN��O
select v.nome as 'Nome Vendedor', f.nome as 'Nome Filial'
from vendedores v, filiais f
where cod = codfil

-- Listar nome dos vendedores com o nome de sua filial de lota��o e uma 
-- simula��o de seu sal�rio com um aumento de 15%
select v.nome as 'Nome Vendedor', f.nome as 'Nome Filial', 
salario*1.15 as 'Sal�rio com Aumento'
from vendedores v, filiais f

-- Listar nome dos fornecedores que tiveram itens vendidos por vendedores que ganham
-- mais que 4500
select distinct f.nome
from fornecedores f, estoque e, historico, vendedores 
where f.cod = codfor and e.cod=coditem and
matrvend = matr and salario > 4500

-- Listar vendedores com sal�rios maior que 1000 e menor que 2000
select v.nome 
from vendedores v
where salario > 1000 and salario < 2000

-- Listar nome do vendedor e o nome de sua filial de lota��o, para todos vendedores
-- que ganham mais que 4500
select v.nome, f.nome
from vendedores v, filiais f
where v.salario > 4500

-- DESAFIO
-- Listar nome das filiais que venderam itens de fornecedores localizados na cidade de S�o Paulo:
-- Romulo(521353) Maria Leticia(536318)
select distinct fi.nome as 'Filial'
from fornecedores f, estoque e, historico, vendedores, filiais fi
where f.cod = codfor and e.cod = coditem and matrvend = matr and codfil = fi.cod 
and f.cid like '__o Paulo'

-- Listar matr�cula dos vendedores que n�o possuem vendas
Select matr From vendedores 
EXCEPT
Select matrvend From historico

-- Listar matr�cula dos vendedores que tiveram vendas
Select matr From vendedores
INTERSECT
Select matrvend From historico

-- Listar vendedores ordenados por sal�rio na ordem decrescente e por nome na ordem crescente
Select * 
from vendedores
order by salario desc, nome

-- Encontre o n�mero de vendedores lotados na filial Recife
select count(*) from vendedores e, filiais d
where e.codfil=d.cod and d.cid like 'Recife'

-- Encontre o n�mero de vendedores lotados na filial Recife
select count(*) 
from vendedores, filiais
where cod = codfil and cid like '_ortaleza'

-- Encontre o montante da folha de pagamento da empresa
select sum(salario) from vendedores

-- montante de sal�rios distintos
select sum(distinct salario) from vendedores 

-- Encontre o sal�rio m�dio pago pela empresa
select cast (avg(distinct salario) as decimal(6,2))
from vendedores
select cast(avg(salario) as decimal(6,2)) from vendedores

-- Calcular a m�dia salarial de cada filial pelo c�digo
select f.nome, avg(salario)
from vendedores, filiais f
where cod=codfil
group by cod, f.nome 