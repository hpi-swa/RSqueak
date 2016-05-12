example
example
	| db cursor item |
	FileStream startUp: true.
	db := SQLDatabase open: '/Users/fniephaus/code/RSqueak/images/sqpyte/RSqueak.db'.
	cursor := db execute: SQPyte testJoin.
	FileStream stdout nextPutAll: 'Start'; cr.
	[ item := cursor next ]
		whileNotNil: [ FileStream stdout nextPutAll: item asString; cr ].
	FileStream stdout nextPutAll: 'Done'; cr.
	db close