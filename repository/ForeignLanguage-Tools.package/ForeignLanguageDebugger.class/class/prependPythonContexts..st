pyContext
prependPythonContexts: aContext
	| currentPyFrame newFrames |
	currentPyFrame := Python primGetTopFrame.
	currentPyFrame ifNil: [ ^ aContext ].
	newFrames := OrderedCollection new.
	[ currentPyFrame notNone ]
		whileTrue: [
			newFrames add: (self newForeignContext: currentPyFrame).
			currentPyFrame := currentPyFrame f_back ].
	newFrames overlappingPairsDo: [ :a :b |
		a sender: b ].
	newFrames last sender: aContext.
	^ newFrames first