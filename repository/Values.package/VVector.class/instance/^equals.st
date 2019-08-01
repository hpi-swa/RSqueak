comparing
= otherCollection

	^ self species == otherCollection species 
	and: [self hasEqualElements: otherCollection]