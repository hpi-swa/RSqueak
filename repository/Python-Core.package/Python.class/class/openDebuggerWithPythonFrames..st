system primitives
openDebuggerWithPythonFrames: pyError
	| resumeCtx error |
	resumeCtx := thisContext sender.
	[ resumeCtx method selector = #resume:
		and: [ resumeCtx closure isNil ] ]
			whileFalse: [
			resumeCtx := resumeCtx sender.
			resumeCtx ifNil: [ ^ self error: 'resumeCtx not found' ] ].
	error := pyError asSmalltalk.
	ForeignLanguage
		openOn: Processor activeProcess
		context: (ForeignLanguage prependPythonContexts: resumeCtx)
		label: error first, ': ', error second
		contents: nil
		fullView: true