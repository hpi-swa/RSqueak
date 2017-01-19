as yet unclassified
styleInBackgroundProcess: aText
	Python vmSpeaksPython ifTrue: [^ aText].
	^ super styleInBackgroundProcess: aText