pyContext
prependPythonContexts: aContext
	| currentPyFrame newFrames |
	currentPyFrame := Python primGetTopFrame.
	currentPyFrame ifNil: [ ^ aContext ].
	newFrames := OrderedCollection new.
	[ currentPyFrame notNil ]
		whileTrue: [ | newCtx |
			newCtx := self newPyContext.
			newCtx pyFrame: currentPyFrame.
			newFrames add: newCtx.
			currentPyFrame := currentPyFrame f_back ].
	newFrames overlappingPairsDo: [ :a :b |
		a sender: b ].
	newFrames last sender: aContext.
	^ newFrames first