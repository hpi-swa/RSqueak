as yet unclassified
sourceCodeAt: selector ifAbsent: aBlock
	^ self pyMethodDict at: selector ifAbsent: [
		^ super sourceCodeAt: selector ifAbsent: aBlock ]