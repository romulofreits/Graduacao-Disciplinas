-- Retorna todo o conteúdo da tabela:
-- select * from nome_tabela

-- Selecionar apenas alguns campos da tabela
-- select nome_coluna, nome_coluna from nome_tabela

-- Utilizar Alias (APELIDO) para Nome da Tabela
-- select Nome_Tabela T

-- Utilizar nomes personalizados para nossos campos
-- Select T.id_curso AS IDC, T.id_turma AS IDT from dbo.turmas T

-- Segunda forma:
-- Select tt.id_curso IDC, tt.id_turma IDT, tt.data_inicio "DATA COMEÇO" from dbo.turmas TT