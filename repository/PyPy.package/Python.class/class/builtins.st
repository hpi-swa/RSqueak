special objects
builtins

	^ PythonBuiltins ifNil: [PythonBuiltins := self eval: '__builtins__']