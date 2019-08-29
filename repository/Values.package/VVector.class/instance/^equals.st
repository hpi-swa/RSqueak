comparing
= otherCollection

	" value semantics "
	^ otherCollection class == self class
	and: [otherCollection size == self size
	and: [self size = 0
		or: [self hasEqualElements: otherCollection]]]