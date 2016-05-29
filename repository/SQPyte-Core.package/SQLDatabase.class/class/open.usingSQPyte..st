instance creation
open: aFilename usingSQPyte: aBool
	| pointer |
	pointer := self primitiveSQLConnect: aFilename usingSQPyte: aBool.
	aBool
		ifTrue: [ FileStream stdout nextPutAll: 'Using SQPyte...'; cr ]
		ifFalse: [ FileStream stdout nextPutAll: 'Using SQLite'; cr ].
	^ self new
		dbPointer: pointer;
		yourself