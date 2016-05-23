*SQPyte-Core
testInnerJoinLite
	| db time |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/tpch.db' with: SQLiteDriver.
	time := [ 100 timesRepeat: [ | cursor item |
			cursor := db execute: SQPyte testInnerJoin.
			[ item := cursor next ] whileNotNil: [ ].
			FileStream stdout nextPut: $..
		]] timeToRun.
	FileStream stdout nextPutAll: time asString; cr.
	db close