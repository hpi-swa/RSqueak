as yet unclassified
sourceCodeAt: selector ifAbsent: aBlock
	Python vmSpeaksPython ifFalse: [ ^ super sourceCodeAt: selector ifAbsent: aBlock ].
	^ self codeAt: selector ifAbsent: aBlock
		