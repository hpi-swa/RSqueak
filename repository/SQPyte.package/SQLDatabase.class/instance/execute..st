database
execute: aSql
	| pointer |
	pointer := (self class primitiveSQLExecute: self dbPointer with: aSql).
	^ SQLCursor for: pointer