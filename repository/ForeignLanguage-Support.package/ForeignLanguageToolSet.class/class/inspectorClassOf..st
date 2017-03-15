inspecting
inspectorClassOf: anObject
	anObject isForeign ifTrue: [ ^ ForeignLanguageInspector ].
	^ super inspectorClassOf: anObject