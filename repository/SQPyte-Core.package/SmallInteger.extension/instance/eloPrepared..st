*SQPyte-Core
eloPrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLElo testPreparedElo: args second asInteger usingSQPyte: (SQPyte check: args first).
	