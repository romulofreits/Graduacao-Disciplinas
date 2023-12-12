-- Listar nome da filial com empregados ganhando duas vezes mais que a média salarial da filial
select filiais.nome
from filiais
where exists (
select * 
from vendedores
where vendedores.codfil = filiais.cod and salario > (2*(select avg(salario)
from vendedores
where vendedores.codfil = filiais.cod)))

-- Listar nome do fornececedor, para o qual não houve venda de seus produtos
select fornecedores.nome
from fornecedores
where not exists (select *
from estoque, historico
where fornecedores.cod = estoque.codfor and estoque.cod = historico.coditem)