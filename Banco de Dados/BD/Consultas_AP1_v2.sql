--	QUESTÃO 1

/*SELECT Filiais.nome AS 'nome_filial', SUM(Vendas.qtd_vendas * Vendas.prvenda) AS 'volume_financeiro'
FROM Filiais
JOIN Vendedores ON Filiais.codfil = Vendedores.codfil
JOIN Vendas ON Vendedores.matr = Vendas.matr
GROUP BY Filiais.nome
ORDER BY volume_financeiro DESC;*/

SELECT filiais.nome AS 'nome_filial', SUM(historico.qtde * historico.prven) AS 'volume_financeiro'
FROM filiais
JOIN vendedores ON filiais.cod = vendedores.codfil
JOIN historico ON vendedores.matr = historico.matrvend
GROUP BY filiais.nome
ORDER BY [volume_financeiro] DESC

-- QUESTÃO 2

/*SELECT Fornecedor.nome AS nome_fornecedor, Filiais.nome AS nome_filial, SUM(Vendas.qtd_vendas) AS quantidade_itens_vendidos
FROM Fornecedor
JOIN Estoque ON Fornecedor.codfor = Estoque.codfor
JOIN Vendas ON Estoque.coditem = Vendas.coditem
JOIN Vendedores ON Vendas.matr = Vendedores.matr
JOIN Filiais ON Vendedores.codfil = Filiais.codfil
GROUP BY Fornecedor.nome, Filiais.nome
ORDER BY nome_fornecedor, nome_filial;*/

SELECT fornecedores.nome AS 'nome_fornecedor', filiais.nome AS 'nome_filial', SUM(historico.qtde) AS 'quantidade_itens_vendidos'
FROM fornecedores
JOIN estoque ON fornecedores.cod = estoque.codfor
JOIN historico ON estoque.cod = historico.coditem
JOIN vendedores ON historico.matrvend = vendedores.matr
JOIN filiais ON vendedores.codfil = filiais.cod
GROUP BY fornecedores.nome, filiais.nome
ORDER BY nome_fornecedor, nome_filial;

-- QUESTÃO 3

/* SELECT Fornecedor.nome AS nome_fornecedor, Vendedores.nome AS nome_vendedor, SUM(Vendas.qtd_vendas) AS quantidade_itens_vendidos
FROM Fornecedor
JOIN Estoque ON Fornecedor.codfor = Estoque.codfor
JOIN Vendas ON Estoque.coditem = Vendas.coditem
JOIN Vendedores ON Vendas.matr = Vendedores.matr
GROUP BY Fornecedor.nome, Vendedores.nome
ORDER BY nome_fornecedor, nome_vendedor; */

SELECT fornecedores.nome AS 'nome_fornecedor', vendedores.nome AS 'nome_vendedor', SUM(historico.qtde) AS 'quantidade_itens_vendidos'
FROM fornecedores
JOIN estoque ON fornecedores.cod = estoque.codfor
JOIN historico ON estoque.cod = historico.coditem
JOIN vendedores ON historico.matrvend = vendedores.matr
GROUP BY fornecedores.nome, vendedores.nome
ORDER BY nome_fornecedor, nome_vendedor;



