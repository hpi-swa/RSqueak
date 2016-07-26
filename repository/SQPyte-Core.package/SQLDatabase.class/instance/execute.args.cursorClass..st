database
execute: aSql args: anArgumentArray cursorClass: aClass
	| pointer |
	pointer := self class primitiveSQLExecute: self dbPointer with: aSql args: anArgumentArray.
	^ aClass for: pointer