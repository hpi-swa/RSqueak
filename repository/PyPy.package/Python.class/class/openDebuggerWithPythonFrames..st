system primitives
openDebuggerWithPythonFrames: pyError
	PythonDebugger
		openOn: Processor activeProcess
		context: (PythonDebugger appendPythonContexts: thisContext)
		label: pyError class asString, ':  ', pyError asString
		contents: nil
		fullView: true