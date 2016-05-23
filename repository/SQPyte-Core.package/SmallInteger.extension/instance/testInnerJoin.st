*SQPyte-Core
testInnerJoin
	| db time |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/tpch.db'.
	time := [ 100 timesRepeat: [ | cursor item |
			cursor := db execute: SQPyte testInnerJoin.
			[ item := cursor next ] whileNotNil: [ ].
			FileStream stdout nextPut: $..
		]] timeToRun.
	FileStream stdout nextPutAll: time asString; cr.
	db close