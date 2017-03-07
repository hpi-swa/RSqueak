as yet unclassified
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock
	Python vmSpeaksPython ifTrue: [ self pythonCompile: textOrStream class: aClass ifFail: failBlock ].
	^ super compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock