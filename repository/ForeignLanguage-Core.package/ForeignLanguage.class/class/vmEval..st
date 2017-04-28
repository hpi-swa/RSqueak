special methods
vmEval: languageProcess
	[ ^ self vmResume: languageProcess ] on: Error do: [
		self checkForException: languageProcess.
		^ self vmEval: languageProcess ]