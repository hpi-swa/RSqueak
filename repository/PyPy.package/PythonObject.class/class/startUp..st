as yet unclassified
startUp: resuming
	(resuming and: [ Python vmSpeaksPython ])
		ifTrue: [ self pythonInitialize ]