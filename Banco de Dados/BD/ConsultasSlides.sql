-- Consultas do banco de dados lojas:

-- Listar o nome e o salário de todos os vendedores lotados na filial 01. 
-- No resultado, os nomes das colunas dvem ser Nome do Vendedor e Salário Líquido
select nome as 'Nome do Vendedor', salario as 'Salario Líquido'
from vendedores
where codfil = 01

-- Listar o código dos itens que tiveram vendas
select distinct coditem
from historico

-- Listar todos os vendedores com sobrenome 'brayner'
select nome from vendedores
where nome like '%brayner%'

-- JUNÇÃO
select v.nome as 'Nome Vendedor', f.nome as 'Nome Filial'
from vendedores v, filiais f
where cod = codfil

-- Listar nome dos vendedores com o nome de sua filial de lotação e uma 
-- simulação de seu salário com um aumento de 15%
select v.nome as 'Nome Vendedor', f.nome as 'Nome Filial', 
salario*1.15 as 'Salário com Aumento'
from vendedores v, filiais f

-- ChatGPT
SELECT v.nome AS Vendedor, f.nome AS Filial, v.salario * 1.15 AS SalarioSimulado
FROM vendedores v
JOIN filiais f ON v.codfil = f.cod;

-- Listar nome dos fornecedores que tiveram itens vendidos por vendedores que ganham
-- mais que 4500
select distinct f.nome
from fornecedores f, estoque e, historico, vendedores 
where f.cod = codfor and e.cod=coditem and
matrvend = matr and salario > 4500

-- Listar vendedores com salários maior que 1000 e menor que 2000
select v.nome 
from vendedores v
where salario > 1000 and salario < 2000

-- Listar nome do vendedor e o nome de sua filial de lotação, para todos vendedores
-- que ganham mais que 4500
SELECT DISTINCT forn.nome AS Fornecedor
FROM fornecedores forn
JOIN estoque est ON forn.cod = est.codfor
JOIN historico hist ON est.cod = hist.coditem
JOIN vendedores vend ON hist.matrvend = vend.matr
WHERE vend.salario > 4500;


-- DESAFIO
-- Listar nome das filiais que venderam itens de fornecedores localizados na cidade 
-- de São Paulo:
-- Romulo(521353) Maria Leticia(536318)
select distinct fi.nome as 'Filial'
from fornecedores f, estoque e, historico, vendedores, filiais fi
where f.cod = codfor and e.cod = coditem and matrvend = matr and codfil = fi.cod 
and f.cid like '__o Paulo'

SELECT DISTINCT filiais.nome AS 'Filial'
FROM filiais
INNER JOIN vendedores ON filiais.cod = vendedores.codfil
INNER JOIN historico ON vendedores.matr = historico.matrvend
INNER JOIN estoque ON historico.coditem = estoque.cod
INNER JOIN fornecedores ON estoque.codfor = fornecedores.cod
WHERE fornecedores.cid = 'São Paulo'


SELECT vendedores.matr AS 'Matricula'
FROM vendedores
WHERE NOT EXISTS (
  SELECT 1
  FROM historico
  WHERE historico.matrvend = vendedores.matr
);

select vendedores.matr
from vendedores
except 
select matrvend
from historico

select vendedores.matr
from vendedores
intersect 
select matrvend
from historico

-- Listar matrícula dos vendedores que não possuem vendas
Select matr From vendedores 
EXCEPT
Select matrvend From historico

-- Listar matrícula dos vendedores que tiveram vendas
Select matr From vendedores
INTERSECT
Select matrvend From historico

-- Listar vendedores ordenados por salário na ordem decrescente e por nome na ordem crescente
Select * 
from vendedores
order by salario desc, nome

-- Encontre o número de vendedores lotados na filial Recife
select count(*) from vendedores e, filiais d
where e.codfil=d.cod and d.cid like 'Recife'

SELECT COUNT(*) AS 'NumeroVendedores'
FROM vendedores
JOIN filiais ON vendedores.codfil = filiais.cod
WHERE filiais.nome = 'Recife';

-- Encontre o número de vendedores lotados na filial Recife
select count(*) 
from vendedores, filiais
where cod = codfil and cid like '_ortaleza'

-- Encontre o montante da folha de pagamento da empresa
select sum(salario) from vendedores

-- montante de salários distintos
select sum(distinct salario) from vendedores 

-- Encontre o salário médio pago pela empresa
select cast (avg(distinct salario) as decimal(6,2))
from vendedores
select cast(avg(salario) as decimal(6,2)) from vendedores

SELECT AVG(distinct salario) AS 'SalarioMedio'
FROM vendedores; -- Tem que usar o distinct

-- Calcular a média salarial de cada filial pelo código
select f.nome, avg(salario)
from vendedores, filiais f
where cod=codfil
group by cod, f.nome

select codfil, matr, count(*)
from vendedores
group by codfil, matr

select codfil as 'filial', count(*) as 'numero'
from vendedores
group by codfil

-- Listar maiores e menores salarios de cada filial
select filiais.nome as 'Filial', max(vendedores.salario) as 'Maior Salário',
min(vendedores.salario) as 'Menor Salário'
from filiais inner join vendedores on filiais.cod = vendedores.codfil
group by filiais.nome, vendedores.codfil

select filiais.nome as 'Filial', max(vendedores.salario) as 'Maior Salário',
min(vendedores.salario) as 'Menor Salário'
from filiais, vendedores
where filiais.cod = vendedores.codfil
group by filiais.nome, vendedores.codfil

SELECT f.nome AS Filial, MAX(v.salario) AS MaiorSalario, MIN(v.salario) AS MenorSalario
FROM filiais f
JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome;

SELECT f.nome AS Filial, 
       (SELECT MAX(salario) FROM vendedores WHERE codfil = f.cod) AS MaiorSalario,
       (SELECT MIN(salario) FROM vendedores WHERE codfil = f.cod) AS MenorSalario
FROM filiais f;

-- Mostrar a quantidade de itens vendidos por vendedor e por item, ordenado pelo nome do vendedor
select vendedores.nome as 'Nome do Vendedor', 
estoque.ref as 'Item', sum(historico.qtde) as 'Unidades Vendidas', count(*) as 'Numero de Vendas'
from estoque, vendedores, historico
where estoque.cod = historico.coditem and historico.matrvend = vendedores.matr
group by vendedores.nome, estoque.ref, historico.coditem
order by vendedores.nome

SELECT v.nome AS 'Vendedor', h.coditem AS Item, SUM(h.qtde) AS 'Quantidade'
FROM vendedores v
JOIN historico h ON v.matr = h.matrvend
GROUP BY v.nome, h.coditem
ORDER BY v.nome ASC;

-- HAVING
-- Listar nome das filiais com média salárial maior que 2000, em ordem decrescente de média salarial
select filiais.nome as 'Filial', AVG(vendedores.salario)
from filiais, vendedores
where filiais.cod = vendedores.codfil
group by filiais.nome
having AVG(vendedores.salario) > 2000
order by AVG(vendedores.salario) desc

SELECT f.nome AS 'Filial', AVG(v.salario) AS 'MediaSalarial'
FROM filiais f
JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome
HAVING AVG(v.salario) > 2000
ORDER BY AVG(v.salario) DESC;

-- Listar nome e média salarial das filiais que possuem mais de 6 vendedores
select filiais.nome, AVG(vendedores.salario) as 'Media Salarial'
from filiais, vendedores
where filiais.cod = vendedores.codfil
group by filiais.nome
having COUNT(vendedores.matr) > 6

SELECT f.nome AS Filial, AVG(v.salario) AS MediaSalarial
FROM filiais f
JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome
HAVING COUNT(v.matr) > 6;

-- Listar o nome e a quantidade de vendedores das filiais cuja média salárial é maior que 5000
select filiais.nome, count(*) as 'NumeroVendedores'
from filiais, vendedores
where filiais.cod = vendedores.codfil
group by filiais.nome
having avg(vendedores.salario) >= 5000

select filiais.nome, count(*) as 'NumeroVendedores'
from filiais inner join vendedores on filiais.cod = vendedores.codfil
group by filiais.nome
having avg(vendedores.salario) >= 5000

SELECT f.nome AS Filial, COUNT(v.matr) AS QuantidadeVendedores
FROM filiais f
JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome
HAVING AVG(v.salario) > 5000;

/* Gerar relatório com o nome de cada vendedor e quantidade de vendas efetuadas por ele, em 
ordem decrescente de quantidade de vendas. Só devem aparecer no relatório vendedores com
volume de vendas superior a 1.000,00*/
select vendedores.nome as 'NomeVendedor', count(*) as 'TotalVendas'
from vendedores, historico
where vendedores.matr = historico.matrvend
group by vendedores.nome, historico.matrvend
having sum(historico.qtde*historico.prven) > 1000
order by 2 desc

select vendedores.nome as 'NomeVendedor', count(historico.matrvend) as 'TotalVendas'
from vendedores, historico
where vendedores.matr = historico.matrvend
group by vendedores.nome, historico.matrvend
having sum(historico.qtde*historico.prven) > 1000
order by TotalVendas desc

SELECT v.nome AS Vendedor, COUNT(*) AS QuantidadeVendas
FROM vendedores v
JOIN historico h ON v.matr = h.matrvend
GROUP BY v.nome
HAVING SUM(h.qtde * h.prven) > 1000.00
ORDER BY QuantidadeVendas DESC;

-- Listar o primeiro e o segundo maiores salários da empresa
select max(vendedores.salario)
from vendedores
union
select max(vendedores.salario)
from vendedores
where vendedores.salario <> (select max(vendedores.salario)
from vendedores)

-- listar filial com maior media salarial
select filiais.nome 
from vendedores, filiais
where filiais.cod = vendedores.codfil
group by filiais.nome
having avg(vendedores.salario) >= all (
select avg(vendedores.salario) from vendedores group by vendedores.codfil)

-- Listar referência, quantidade e preço de compra de itens sem movimentação de saída
-- ordenado de forma decrescente, por preço de compra
select estoque.ref, estoque.qtde, estoque.prcom
from estoque
where not exists (
select estoque.cod from estoque inner join historico on estoque.cod = historico.coditem
where estoque.cod = historico.coditem)
order by prcom

SELECT e.ref AS Referencia, e.qtde AS Quantidade, e.prcom AS PrecoCompra
FROM estoque e
LEFT outer JOIN historico h ON e.cod = h.coditem
WHERE h.coditem IS NULL
ORDER BY e.prcom DESC;

-- Listar referência, preço de compra e numero de vendas de cada item de estoque. Ordenado de 
-- Forma decrescente, por numero de vendas
select estoque.ref, estoque.prcom, count(historico.coditem)
from estoque left outer join historico on estoque.cod = historico.coditem
group by coditem, ref, prcom
order by prcom desc

SELECT e.ref AS Referencia, e.prcom AS PrecoCompra, COUNT(h.coditem) AS NumeroVendas
FROM estoque e
LEFT JOIN historico h ON e.cod = h.coditem
GROUP BY e.ref, e.prcom
ORDER BY COUNT(h.coditem) DESC;





