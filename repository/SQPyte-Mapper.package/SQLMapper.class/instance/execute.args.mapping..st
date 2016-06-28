database
execute: aSQLQueryString args: anArgsList mapping: aClass
	| query |
	query := self database execute: aSQLQueryString args: anArgsList.
	
	^ query