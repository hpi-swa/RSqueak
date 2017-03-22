debugging
openDebuggerOn: foreignError languageProcess: languageProcess
	| resumeCtx |
	resumeCtx := self findResumeFrame: thisContext sender.
	ForeignLanguageDebugger
		openOn: Processor activeProcess
		context: (self prependContexts: resumeCtx languageProcess: languageProcess)
		label: (self debuggerTitle: foreignError)
		contents: nil
		fullView: true