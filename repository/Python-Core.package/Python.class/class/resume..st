special methods
resume: pyProcess
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume: pyProcess ] on: Error do: [
		self checkForException: pyProcess.
		^ self resume: pyProcess ]