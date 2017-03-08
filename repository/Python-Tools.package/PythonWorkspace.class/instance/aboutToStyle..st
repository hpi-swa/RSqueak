overrides
aboutToStyle: aStyler
	(super aboutToStyle: aStyler) ifFalse: [ ^ false ].
	aStyler isPythonCode: true.
	^ true