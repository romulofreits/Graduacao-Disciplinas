-- SUBCONSULTAS

-- Listar vendedores com salário maior que a média salarial da empresa
select vendedores.nome as 'Vendedor', vendedores.salario as 'Salário'
from vendedores
where salario > (select avg(salario) from vendedores)

-- Listar o primeiro e segundo maiores salários da empresa
select max(salario) from vendedores
union
select max(salario) from Vendedores 
where salario <> (select max(salario) from vendedores)
 --
 select max(salario) as 'maior salario',
(select max(salario) from vendedores where salario <> (select max(salario) 
from vendedores)) as 'segundo maior salário' from Vendedores

-- Listar nome das filiais com média salarial maior que a média salarial da empresa
select filiais.nome, avg(vendedores.salario)
from filiais, vendedores
where filiais.cod = vendedores.codfil
group by vendedores.codfil, filiais.nome
having avg(vendedores.salario) > (select avg(salario) from vendedores)

SELECT f.nome AS 'Filial', AVG(v.salario) AS 'MediaSalarial'
FROM filiais f
INNER JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome
HAVING AVG(v.salario) > (
    SELECT AVG(salario)
    FROM vendedores
)
ORDER BY f.nome;

SELECT f.nome AS 'Fornecedor'
FROM fornecedores f
LEFT JOIN estoque e ON f.cod = e.codfor
WHERE e.cod IS NULL;

select f.nome
from fornecedores f
where not exists (select * from estoque e, historico
where f.cod=codfor and e.cod=coditem)

SELECT f.nome AS 'Fornecedor'
FROM fornecedores f
WHERE NOT EXISTS (
    SELECT *
    FROM estoque e
    WHERE f.cod = e.codfor
);

SELECT f.nome AS 'Filial'
FROM filiais f
INNER JOIN vendedores v ON f.cod = v.codfil
GROUP BY f.nome
HAVING MAX(v.salario) >= 2 * AVG(v.salario);

select f.nome
from filiais f
where exists (select * from vendedores v
where v.codfil=f.cod and
salario > (2*(select avg(salario) from vendedores 
where codfil=v.codfil)))








