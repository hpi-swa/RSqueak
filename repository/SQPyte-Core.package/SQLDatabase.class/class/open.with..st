instance creation
open: aFilename with: aDriver
	| pointer |
	pointer := aDriver primitiveSQLConnect: aFilename.

	^ self new
		dbDriver: aDriver;
		dbPointer: pointer;
		yourself