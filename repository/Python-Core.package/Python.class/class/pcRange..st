debugging
pcRange: pyFrame
	| relativeLine lineCount |
	relativeLine := pyFrame f_lineno asSmalltalk - pyFrame f_code co_firstlineno asSmalltalk.
	lineCount := 0.
	(Python getSource: pyFrame) lineIndicesDo: [:start :endWithoutDelimiters :end |
		(lineCount := lineCount + 1) = (relativeLine + 1)
			ifTrue: [ ^ start to: end ]].
	^ 1 to: 0