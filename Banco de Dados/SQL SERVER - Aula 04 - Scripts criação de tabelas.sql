CREATE TABLE Alunos
(
	id_aluno int PRIMARY KEY NOT NULL,
	nome varchar(200) NOT NULL,
	data_nascimento date NOT NULL,
	sexo varchar (1) NOT NULL,
	data_cadastro datetime NOT NULL,
	login_cadastro varchar(15) NOT NULL
);

--drop table Alunos;

CREATE TABLE Situacao
(
	id_situacao int PRIMARY KEY NOT NULL,
	situacao varchar(25) not null,
	data_cadastro datetime,
	ogin_cadastro varchar(15)
);

create table Cursos
(
	id_curso int PRIMARY KEY NOT NULL,
	nome_curso varchar(200) not null,
	data_cadastro datetime not null,
	login_cadastro varchar(15)
);

create table Turmas
(
	id_turma int PRIMARY KEY not null,
	id_aluno int not null,
	id_curso int not null,
	valor_turma numeric(15,2) not null,
	desconto numeric(3,2) not null,
	data_inicio date not null,
	data_termino date,
	data_cadastro datetime not null,
	login_cadatro varchar(15)
);

create table Registro_Presenca
(
	id_turma int not null,
	id_aluno int not null, 
	id_situacao int not null,
	data_presenca date not null,
	data_cadastro date not null,
	login_cadastro varchar(15) not null
);

/*drop table Turmas;
drop table Alunos;
drop table Situacao;
drop table Registro_Presenca;
drop table Cursos;
*/

-- Turmas X Alunos
ALTER TABLE Turmas 
	ADD CONSTRAINT fk_TurmasAlunos FOREIGN KEY (id_aluno)
	REFERENCES Alunos(id_aluno);

-- Turmas X Cursos
ALTER TABLE Turmas
	ADD CONSTRAINT fk_TurmasCursos FOREIGN KEY (id_curso)
	REFERENCES Cursos(id_curso);

-- Registro_Presenca X Turmas
ALTER TABLE Registro_Presenca
	add constraint fk_RPTurmas foreign key (id_turma)
	references Turmas(id_turma);

-- Registro_Presenca X Alunos
ALTER TABLE Registro_Presenca
	add constraint fk_RPAlunos foreign key (id_aluno)
	references Alunos(id_aluno);

-- Registro_Presenca X Situacao
ALTER TABLE Registro_Presenca
	add constraint fk_RPSituacao foreign key (id_situacao)
	references Situacao(id_situacao);