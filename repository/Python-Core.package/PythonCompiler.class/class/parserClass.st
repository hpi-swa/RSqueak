as yet unclassified
parserClass
	Python vmSpeaksLanguage ifFalse: [ ^ Parser ].
	^ PythonParser "PythonParser"
		