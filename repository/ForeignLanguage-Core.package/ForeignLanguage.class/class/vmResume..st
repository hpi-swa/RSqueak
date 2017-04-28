special methods
vmResume: languageProcess
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	^ self primResume: languageProcess