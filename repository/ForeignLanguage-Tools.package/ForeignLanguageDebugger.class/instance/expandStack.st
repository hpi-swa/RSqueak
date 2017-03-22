overrides
expandStack
	super expandStack.
	receiverInspector := ForeignLanguageInspector inspect: nil.
	contextVariablesInspector := ForeignLanguageContextVariablesInspector inspect: nil.