pySource
scopeEndIn: aString startingAt: aLineno
	| lines currentIndentSize end |
	lines := aString lines allButFirst: aLineno.
	currentIndentSize := self indentSize: lines first.
	end := lines allButFirst findFirst: [ :line | (self indentSize: line) < currentIndentSize ].
	end = 0 ifTrue: [ end := lines size ].
	^ aLineno + end