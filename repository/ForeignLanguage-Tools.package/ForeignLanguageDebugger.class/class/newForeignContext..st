pyContext
newForeignContext: foreignFrame
	| frame |
	frame := ForeignLanguageMethodContext
			sender: nil
			receiver: PythonObject
			method: (CompiledMethod newMethod: 2 header: 2)
			arguments: #().
	frame foreignFrame: foreignFrame.
	^ frame