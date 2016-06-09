as yet unclassified
testSingleStatementInsert
	| database query |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	database execute: 'INSERT INTO records VALUES (?, ?, ?)' args: #(1 'John Smith' 12.5).
	query := database execute: 'SELECT * FROM records'.
	
	self assert: query next equals: #(1 'John Swmith' 12.5).
	self assert: query next equals: nil.