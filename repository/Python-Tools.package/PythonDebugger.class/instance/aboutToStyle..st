overrides
aboutToStyle: aStyler
	self isModeStyleable ifFalse: [^false].
	self hasPyContextSelected ifFalse: [^ super aboutToStyle: aStyler].
	aStyler
		classOrMetaClass: self selectedClassOrMetaClass;
		isPythonCode: self hasPyContextSelected.
	^ true