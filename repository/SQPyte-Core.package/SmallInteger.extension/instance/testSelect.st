*SQPyte-Core
testSelect
	| db time |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/tpch.db'.
	time := [ 100 timesRepeat: [ | cursor item |
			cursor := db execute: SQPyte testSelect.
			[ item := cursor next ] whileNotNil: [ ].
			FileStream stdout nextPut: $..
		]] timeToRun.
	FileStream stdout nextPutAll: time asString; cr.
	db close