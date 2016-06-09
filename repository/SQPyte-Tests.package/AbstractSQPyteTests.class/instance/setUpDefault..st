as yet unclassified
setUpDefault: database
	database execute: 'CREATE TABLE records (id INT PRIMARY KEY, name TEXT, salary REAL)'