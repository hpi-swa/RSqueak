as yet unclassified
newFor: foreignFrame
	| frame |
	frame := FLMethodContext
			sender: nil
			receiver: PythonObject
			method: (CompiledMethod newMethod: 2 header: 2)
			arguments: #().
	frame foreignFrame: foreignFrame.
	^ frame