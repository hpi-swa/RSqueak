*SQPyte-Core
elo: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLElo testElo: (Smalltalk classNamed: args first) match: args second asInteger
	