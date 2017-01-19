overrides
isModeStyleable
	"determine the current mode can be styled"
	^ super isModeStyleable and: [self selectedContext isPyContext not]