database
execute: aSql args: anArgumentArray
	^ self execute: aSql args: anArgumentArray cursorClass: SQLCursor