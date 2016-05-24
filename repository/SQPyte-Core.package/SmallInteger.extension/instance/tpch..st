*SQPyte-Core
tpch: aString
	| args db time |
	args := aString findTokens: ' '.
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/tpch.db' with: (Smalltalk classNamed: args first).
	time := [ args second asInteger timesRepeat: [ | sql cursor item |
			sql := SQPyte class executeMethod: (SQPyte class methodDict at: (args third asSymbol)).
			cursor := db execute: sql.
			[ item := cursor next ] whileNotNil: [ ].
			FileStream stdout nextPut: $..
		]] timeToRun.
	FileStream stdout nextPutAll: time asString, 'ms'; cr.
	db close