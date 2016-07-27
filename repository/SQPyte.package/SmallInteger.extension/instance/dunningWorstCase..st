*SQPyte-Core
dunningWorstCase: aString
	| args |
	args := aString findTokens: ' '.
	^ SQLDunning worstCaseIn: args second usingSQPyte: (SQPyte check: args first)