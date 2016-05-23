*SQPyte-Core
testSelectDebug
	| db results time |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/tpch.db'.
	results := 0.
	time := [ 1 timesRepeat: [ | cursor item |
			cursor := db execute: SQPyte testSelect.
			MessageTally spyOn: [ [ item := cursor next ] whileNotNil: [ results := results + 1 ] ].
			FileStream stdout nextPut: $..
		]] timeToRun.
	FileStream stdout nextPutAll: 'Results: ', results asString; cr.
	FileStream stdout nextPutAll: time asString; cr.
	db close