special methods
vmResume
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	^ self primResume