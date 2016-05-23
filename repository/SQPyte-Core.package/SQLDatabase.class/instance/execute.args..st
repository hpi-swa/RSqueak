database
execute: aSql args: anArgumentArray
	| pointer |
	pointer := self dbDriver primitiveSQLExecute: self dbPointer with: aSql args: anArgumentArray.

	^ SQLCursor for: pointer with: self dbDriver