as yet unclassified
testQueryColumnNameAt
	| database query |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	query := database execute: 'SELECT * FROM employees'.
	
	self assert: (query columnNameAt: 1) equals: 'id'.
	self assert: (query columnNameAt: 2) equals: 'name'.
	self assert: (query columnNameAt: 3) equals: 'salary'.
