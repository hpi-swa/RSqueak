as yet unclassified
inspectorClassOf: anObject
	anObject isPython ifFalse: [ ^ super inspectorClassOf: anObject ].
	^ PythonInspector