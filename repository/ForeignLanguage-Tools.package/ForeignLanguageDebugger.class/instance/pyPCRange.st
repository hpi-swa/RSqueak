overrides
pyPCRange
	| pyFrame relativeLine lineCount |
	pyFrame := self selectedContext foreignFrame.
	relativeLine := pyFrame f_lineno asSmalltalk - pyFrame f_code co_firstlineno asSmalltalk.
	lineCount := 0.
	(self class getPySource: self selectedContext) lineIndicesDo: [:start :endWithoutDelimiters :end |
		(lineCount := lineCount + 1) = (relativeLine + 1)
			ifTrue: [ ^ start to: end ]].
	^1 to: 0