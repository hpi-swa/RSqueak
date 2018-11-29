overrides
pyCompile: text classified: category notifying: requestor
	| pySelector selector pySource isNewMethod |
	pySource := text asString.
	pySelector := PythonCompiler extractPySelector: pySource.
	selector := pySelector asSymbol.
	isNewMethod := self pyMethodDict includesKey: pySelector.
	Python exec: pySource, String cr,
		self name asString, '.', pySelector, ' = ', pySelector.
	self pyMethodDict at: selector put: pySource.
	self organization classify: selector under: category suppressIfDefault: false.
	^ pySelector