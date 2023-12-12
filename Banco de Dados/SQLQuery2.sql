-- Retorna o montante da folha de pagamento da empresa
select sum(salario) from vendedores

-- Retorna o salário médio pago pela empresa 
select cast(avg(distinct salario) as decimal(6,2)) from vendedores

-- Encontre o número de vendedores lotados na filial recife
select count(*) from vendedores v, filiais f 
where v.codfil = f.cod and f.cid like 'Recife'