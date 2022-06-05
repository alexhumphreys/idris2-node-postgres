-- from https://www.educba.com/postgresql-select/

CREATE TABLE IF NOT EXISTS educba
(id INTEGER PRIMARY KEY,
technologies VARCHAR,
headcount INTEGER,
address VARCHAR);

INSERT INTO educba VALUES (1,'java',20,'satara'),(2,'javascript',30,'mumbai'),(3,'java',20,'satara'),(4,'psql',30,'mumbai'),(5,'mysql',20,'satara'),(6,'maven',30,'mumbai'),(7,'hibernate',20,'satara'),(8,'spring',30,'mumbai'),(9,'angular',20,'satara'),(10,'html',30,'mumbai'),(11,'css',20,'satara'),(12,'reddis',30,'mumbai');

CREATE TABLE IF NOT EXISTS board
(id SERIAL PRIMARY KEY,
country VARCHAR,
total INTEGER);

INSERT INTO board(country,total) VALUES ('sweden',10),('ireland',10);

CREATE TABLE IF NOT EXISTS foo
(id SERIAL PRIMARY KEY,
bar INTEGER,
baz INTEGER);

INSERT INTO foo(bar,baz) VALUES (6,8),(1,7);
