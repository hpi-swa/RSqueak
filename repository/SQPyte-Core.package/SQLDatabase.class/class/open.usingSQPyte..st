instance creation
open: aFilename usingSQPyte: aBool
	| pointer |
	pointer := self primitiveSQLConnect: aFilename usingSQPyte: aBool.
	^ self new
		dbPointer: pointer;
		yourself