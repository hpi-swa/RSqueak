*SQPyte-Core
dunningBetterCasePrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning betterCasePreparedIn: args second usingSQPyte: (SQPyte check: args first)