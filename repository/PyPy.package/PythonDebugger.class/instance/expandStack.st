overrides
expandStack
	"A Notifier is being turned into a full debugger.  Show a substantial amount of stack in the context pane."

	super expandStack.
	receiverInspector := PythonInspector inspect: nil.
	contextVariablesInspector := PythonContextVariablesInspector inspect: nil.