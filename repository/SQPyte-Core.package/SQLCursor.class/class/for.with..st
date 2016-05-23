instance creation
for: aCursorPointer with: aSQLDriver
	^ self new
		dbDriver: aSQLDriver;
		cursorPointer: aCursorPointer;
		yourself