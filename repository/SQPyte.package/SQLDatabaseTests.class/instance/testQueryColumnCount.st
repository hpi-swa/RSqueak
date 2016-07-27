as yet unclassified
testQueryColumnCount
	| database query |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	query := database execute: 'SELECT * FROM employees'.
	
	self assert: query columnCount equals: 3.
