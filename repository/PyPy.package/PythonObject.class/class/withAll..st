as yet unclassified
withAll: anArray
	^ Python eval: self name asString, '(', (anArray joinSeparatedBy: ',') , ')'