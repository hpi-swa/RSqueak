special methods
resume: rbProcess
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume: rbProcess ] on: Error do: [
		self checkForException: rbProcess.
		^ self resume: rbProcess ]