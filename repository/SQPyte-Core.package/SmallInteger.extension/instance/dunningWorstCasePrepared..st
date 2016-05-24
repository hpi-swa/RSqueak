*SQPyte-Core
dunningWorstCasePrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning worstCasePreparedIn: args second with: (Smalltalk classNamed: args first)