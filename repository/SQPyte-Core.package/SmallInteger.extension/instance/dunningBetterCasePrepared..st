*SQPyte-Core
dunningBetterCasePrepared: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning betterCasePreparedIn: args second with: (Smalltalk classNamed: args first)