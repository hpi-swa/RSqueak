private mutation
pvtAt: anIndex put: anObject
	" Will fail if we are immutable. Only for mutable support"
	
	^ super at: anIndex put: anObject