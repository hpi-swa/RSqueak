*SQPyte-Core
eloPrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLElo testPreparedElo: (Smalltalk classNamed: args first) match: args second asInteger.
	