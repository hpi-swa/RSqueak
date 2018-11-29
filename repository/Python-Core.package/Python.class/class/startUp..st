overrides
startUp: resuming
	self reset.
	(resuming and: [ self vmSpeaksLanguage ])
		ifTrue: [
			self setUpPythonEnvironment.
			PythonObject pythonizeAll ]