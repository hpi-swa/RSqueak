helpers
single: aString
	^ self
		primEval: aString
		filename: '<string>'
		cmd: 'single'
		breakOnExceptions: false