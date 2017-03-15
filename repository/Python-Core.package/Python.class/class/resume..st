special methods
resume: aPyLanguage
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume: aPyLanguage ] on: Error do: [
		self checkForException: aPyLanguage.
		^ self resume: aPyLanguage ]