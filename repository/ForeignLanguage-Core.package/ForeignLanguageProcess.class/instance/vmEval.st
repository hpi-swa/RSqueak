special methods
vmEval
	[ ^ self vmResume ] on: Error do: [
		self checkForException.
		^ self vmEval ]