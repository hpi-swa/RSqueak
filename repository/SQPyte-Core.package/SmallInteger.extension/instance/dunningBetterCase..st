*SQPyte-Core
dunningBetterCase: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning betterCaseIn: args second with: (Smalltalk classNamed: args first)