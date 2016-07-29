*SQPyte-Core
dunningBetterCase: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning betterCaseIn: args second usingSQPyte: (SQPyte check: args first)