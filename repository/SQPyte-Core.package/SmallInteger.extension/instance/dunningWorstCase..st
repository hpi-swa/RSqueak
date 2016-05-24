*SQPyte-Core
dunningWorstCase: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning worstCaseIn: args second with: (Smalltalk classNamed: args first)