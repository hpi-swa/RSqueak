overrides
startUp: resuming
	self reset.
	(resuming and: [ self vmSpeaksPython ])
		ifTrue: [
			self setUpPythonEnvironment.
			PythonObject pythonizeAll ]