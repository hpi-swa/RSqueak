as yet unclassified
pythonCompile: pySource class: aClass ifFail: failBlock
	| pySelector |
	pySelector := self class extractPySelector: pySource.
	[ Python exec: pySource, String cr,
		aClass name asString, '.', pySelector, ' = ', pySelector  ]
			on: Error do: failBlock