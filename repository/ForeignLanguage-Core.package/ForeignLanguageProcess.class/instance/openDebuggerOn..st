helpers
openDebuggerOn: foreignError
	| resumeCtx |
	resumeCtx := self findResumeFrame: thisContext sender.
	FLDebugger
		openOn: Processor activeProcess
		context: (self prependContexts: resumeCtx)
		label: (self debuggerTitle: foreignError)
		contents: nil
		fullView: true