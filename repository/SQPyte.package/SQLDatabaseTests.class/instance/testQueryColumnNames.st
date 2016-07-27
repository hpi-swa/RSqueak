as yet unclassified
testQueryColumnNames
	| database query |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	query := database execute: 'SELECT * FROM employees'.
	
	self assert: query columnNames equals: #('id' 'name' 'salary').
