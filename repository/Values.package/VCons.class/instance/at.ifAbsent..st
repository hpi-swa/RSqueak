accessing
at: index ifAbsent: exceptionBlock

	| i |
	index < 1 ifTrue: [^exceptionBlock value].
	i := 0.
	^ self findf: [:ea | (i := i + 1) = index] ifNone: exceptionBlock