as yet unclassified
newPyContext
	^ PythonFakeMethodContext
			sender: nil
			receiver: PythonObject
			method: (PythonFakeCompiledMethod newMethod: 2 header: 2)
			arguments: #()