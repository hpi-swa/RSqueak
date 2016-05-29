database
execute: aSql args: anArgumentArray
	| pointer |
	pointer := self class primitiveSQLExecute: self dbPointer with: aSql args: anArgumentArray.
	^ SQLCursor for: pointer