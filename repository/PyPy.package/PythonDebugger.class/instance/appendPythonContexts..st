helpers
appendPythonContexts: aContext
	| newFrames currentPyFrame |
	newFrames := OrderedCollection new.
	currentPyFrame := Python primGetTopFrame.
	[ currentPyFrame notNil ]
		whileTrue: [ | newCtx |
			newCtx := self newContext.
			newCtx pyFrame: currentPyFrame.
			newFrames add: newCtx.
			currentPyFrame := currentPyFrame f_back ].
	newFrames overlappingPairsDo: [ :a :b |
		a sender: b ].
	newFrames last sender: aContext.
	^ newFrames first