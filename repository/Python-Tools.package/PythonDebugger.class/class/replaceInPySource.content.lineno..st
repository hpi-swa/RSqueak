pysource
replaceInPySource: oldContent content: aString lineno: aLineno
	| lines end indentSize newLines |
	lines := oldContent lines.
	lines size <= 1 ifTrue: [ ^ aString ].
	end := self scopeEndIn: oldContent startingAt: aLineno.
	indentSize := self indentSize: (lines at: aLineno).
	newLines := (self indent: aString by: indentSize) lines.
	lines := lines copyReplaceFrom: aLineno to: end with: newLines.
	^ lines joinSeparatedBy: Character cr