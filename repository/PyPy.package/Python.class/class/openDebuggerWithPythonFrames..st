system primitives
openDebuggerWithPythonFrames: pyError
	| resumeCtx |
	resumeCtx := thisContext sender.
	[ resumeCtx method selector = #resumeFrame
		and: [ resumeCtx closure isNil ] ]
			whileFalse: [
			resumeCtx := resumeCtx sender.
			resumeCtx ifNil: [ ^ self error: 'resumeCtx not found' ] ].
	PythonDebugger
		openOn: Processor activeProcess
		context: (PythonDebugger prependPythonContexts: resumeCtx)
		label: pyError first, ': ', pyError second
		contents: nil
		fullView: true