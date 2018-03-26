as yet unclassified
parseSelector: pyCode
	prevEnd := pyCode indexOf: $:. "highlight method signature"
	^ (PythonCompiler extractPySelector: pyCode) asSymbol