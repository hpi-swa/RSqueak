database
execute: aSQLQueryString args: anArgsList mapping: aClass
	^ (self database execute: aSQLQueryString args: anArgsList cursorClass: SQLMappingCursor)
		mapping: aClass;
		yourself