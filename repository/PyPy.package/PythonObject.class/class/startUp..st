as yet unclassified
startUp: resuming
	Python reset.
	(resuming and: [ Python vmSpeaksPython ])
		ifTrue: [ self pythonInitialize ]