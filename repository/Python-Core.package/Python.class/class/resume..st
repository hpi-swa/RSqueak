special methods
resume: aPyRunner
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume: aPyRunner ] on: Error do: [
		Smalltalk isHeadless ifFalse: [
			self primLastError ifNotNil: [ :e |
				ForeignLanguageException new
					action: [ self openDebuggerWithPythonFrames: e ];
					signal ] ].
		^ self resume: aPyRunner ]