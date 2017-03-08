pyContext
newPyContext
	^ PythonMethodContext
			sender: nil
			receiver: PythonObject
			method: (CompiledMethod newMethod: 2 header: 2)
			arguments: #()