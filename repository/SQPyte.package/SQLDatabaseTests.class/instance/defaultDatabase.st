as yet unclassified
defaultDatabase
	| database |
	database := self createDatabase: ':memory:'.
	
	self setUpDefault: database.
	
	^ database