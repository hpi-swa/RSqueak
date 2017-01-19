as yet unclassified
parseCue: aCue noPattern: noPattern ifFail: aBlock
	| methodNode pySource |
	pySource := aCue sourceStream upToEnd.
	methodNode := self newMethodNode comment: 'Python comment'.
	methodNode
		selector: (PythonCompiler extractPySelector: pySource) asSymbol
		arguments: nil
		precedence: nil
		temporaries: nil
		block: nil
		encoder: nil
		primitive: nil.
	methodNode sourceText: pySource.
	^ methodNode