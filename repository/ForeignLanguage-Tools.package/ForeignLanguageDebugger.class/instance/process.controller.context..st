overrides
process: aProcess controller: aController context: aContext
	| contextForStack |
	super initialize.
	Smalltalk at: #MessageTally ifPresentAndInMemory: [ :tally |
		tally terminateTimerProcess].
	contents := nil. 
	interruptedProcess := aProcess.
	interruptedController := aController.
	contextForStack := self contextForStack: aContext.
	self newStack: (contextForStack stackOfSize: 1).
	contextStackIndex := 1.
	externalInterrupt := false.
	selectingPC := true.
	Smalltalk isMorphic ifTrue:
		[errorWasInUIProcess := false]