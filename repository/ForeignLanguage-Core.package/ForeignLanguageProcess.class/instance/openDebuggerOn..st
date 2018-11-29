helpers
openDebuggerOn: foreignError
	FLDebugger
		openOn: Processor activeProcess
		context: (self findResumeContext: thisContext sender)
		label: (self debuggerTitle: foreignError)
		contents: nil
		fullView: true