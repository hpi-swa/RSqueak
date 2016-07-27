as yet unclassified
setUpDefaultSchema: database
	database execute: 'CREATE TABLE employees (id INT PRIMARY KEY, name TEXT, salary REAL)'.