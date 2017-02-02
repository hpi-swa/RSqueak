pysource
filterPySource: aString lineno: scopeStart
	| lines indentSize end |
	lines := aString lines.
	lines size = 1 ifTrue: [ ^ aString ].
	end := self scopeEndIn: aString startingAt: scopeStart.
	lines := lines copyFrom: scopeStart to: end.
	indentSize := self indentSize: lines first.
	lines := lines collect: [ :ea | ea copyFrom: indentSize + 1 to: ea size ].
	^ lines joinSeparatedBy: Character cr