database
execute: aSql
	| pointer |
	pointer := self dbDriver primitiveSQLExecute: self dbPointer with: aSql.

	^ SQLCursor for: pointer with: self dbDriver