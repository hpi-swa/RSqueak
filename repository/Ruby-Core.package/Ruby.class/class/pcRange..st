debugging
pcRange: rbFrame
	| lineNumber lineCount |
	lineNumber := rbFrame get_lineno asSmalltalk.
	lineCount := 0.
	(Ruby getSource: rbFrame) lineIndicesDo: [:start :endWithoutDelimiters :end |
		(lineCount := lineCount + 1) = lineNumber
			ifTrue: [ ^ start to: end ]].
	^ 1 to: 0