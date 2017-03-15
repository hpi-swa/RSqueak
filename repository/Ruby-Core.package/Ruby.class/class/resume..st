special methods
resume: aRbLanguage
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume: aRbLanguage ] on: Error do: [
		self checkForException: aRbLanguage.
		^ self resume: aRbLanguage ]