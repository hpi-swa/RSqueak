example
example2
	| db cursor time item |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/RSqueak.db'.
	FileStream stdout nextPutAll: 'Start'; cr.
	time := [ 10000 timesRepeat: [
		cursor := db execute: SQPyte testJoin.
		[ item := cursor next ]
			whileNotNil: [ item first ] ] ] timeToRun.
	FileStream stdout nextPutAll: time asString, 'ms'; cr.
	FileStream stdout nextPutAll: 'Done'; cr.
	db close