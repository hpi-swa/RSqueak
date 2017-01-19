overrides
compiledMethodAt: methodSelector ifAbsent: aBlock
	Python vmSpeaksPython ifFalse: [ ^ super compiledMethodAt: methodSelector ifAbsent: aBlock ].
	"Return dummy CM to satisfy browser"
	^ CompiledMethod new methodClass: self; selector: methodSelector; yourself
		
	