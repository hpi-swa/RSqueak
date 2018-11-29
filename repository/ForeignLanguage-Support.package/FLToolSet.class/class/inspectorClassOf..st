inspecting
inspectorClassOf: anObject
	anObject isForeign ifTrue: [ ^ FLInspector ].
	^ super inspectorClassOf: anObject