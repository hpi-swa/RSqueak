overrides
expandStack
	super expandStack.
	receiverInspector := FLInspector inspect: nil.
	contextVariablesInspector := FLContextVariablesInspector inspect: nil.