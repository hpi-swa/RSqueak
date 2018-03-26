as yet unclassified
compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock
	Python vmSpeaksLanguage ifTrue: [ self pythonCompile: textOrStream class: aClass ifFail: failBlock ].
	^ super compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock