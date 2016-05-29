*SQPyte-Core
dunningWorstCasePrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning worstCasePreparedIn: args second usingSQPyte: (SQPyte check: args first)