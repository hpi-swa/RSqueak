helpers
newContext
	^ PythonFakeMethodContext
			sender: nil
			receiver: PythonObject
			method: (PythonFakeCompiledMethod newMethod: 2 header: 2)
			arguments: #()