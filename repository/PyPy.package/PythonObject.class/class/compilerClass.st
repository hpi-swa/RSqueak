as yet unclassified
compilerClass
	Python vmSpeaksPython ifFalse: [ ^ Compiler ].
	^ PythonCompiler
		