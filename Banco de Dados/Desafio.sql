-- DESAFIO
-- Listar nome das filiais que venderam itens de fornecedores localizados na cidade de São Paulo:
-- Romulo(521353) Maria Leticia(536318)

select distinct fi.nome as 'Filial'
from fornecedores f, estoque e, historico, vendedores, filiais fi
where f.cod = codfor and e.cod = coditem and matrvend = matr and codfil = fi.cod 
and f.cid like '__o Paulo'