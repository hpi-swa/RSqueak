instance creation
open: aFilename with: aDriver
	| pointer |
	pointer := aDriver primitiveSQLConnect: aFilename.
	FileStream stdout nextPutAll: 'Using ', aDriver asString; cr.

	^ self new
		dbDriver: aDriver;
		dbPointer: pointer;
		yourself