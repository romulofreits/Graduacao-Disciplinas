-- Listar o nome de filiais, com volume financeiro de vendas 
-- (quantidade de itens vendidos * preço de venda). O resultado deve estar em
-- ordem decrescente de volume financeiro

select filiais.nome, sum(historico.qtde*historico.prven) as 'Volume Finaceiro'
from historico, filiais, vendedores
where filiais.cod = vendedores.codfil and historico.matrvend = vendedores.matr
group by filiais.cod, filiais.nome
order by [Volume Finaceiro] desc

-- Versão ChatGPT
SELECT f.nome AS 'Filial', SUM(h.qtde * h.prven) AS 'VolumeFinanceiro'
FROM filiais f
JOIN vendedores v ON f.cod = v.codfil
JOIN historico h ON v.matr = h.matrvend
JOIN estoque e ON h.coditem = e.cod
GROUP BY f.nome
ORDER BY VolumeFinanceiro DESC;




-- Listar o nome do fornecedor e a quantidade de itens vendidos por filial. No 
-- resultado deve aparecer o nome do fornecedor, o nome da filial e a quantidade de 
-- itens vendidos por vendedor na filial

select fornecedores.nome as 'Fornecedor', filiais.nome as 'Nome da Filial',
sum(historico.qtde) as 'Quantidade Vendida'
from fornecedores, estoque, filiais, vendedores, historico
where fornecedores.cod = estoque.codfor and estoque.cod = historico.coditem
and historico.matrvend = vendedores.matr and vendedores.codfil = filiais.cod
group by fornecedores.cod, fornecedores.nome, filiais.cod, filiais.nome 

-- Listar nome do fornecedor e a quantidade de itens vendidos por vendedor.
-- No resultado deve aparecer o nome do fornecedor, o nome do vendedor e a quantidade
-- de itens do fornecedor vendidos pelo vendedor.

select fornecedores.nome as 'Fornecedor', vendedores.nome as 'Vendedor',
sum(historico.qtde) as 'Quantidade Vendida'
from fornecedores, vendedores, historico, estoque
where fornecedores.cod = estoque.codfor and estoque.cod = historico.coditem 
and historico.matrvend = vendedores.matr
group by fornecedores.nome, fornecedores.cod, vendedores.nome, vendedores.matr