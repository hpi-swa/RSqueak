*SQPyte-Core
elo: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLElo testElo: args second asInteger usingSQPyte: (SQPyte check: args first)