helpers
getForeignFrames: topPyFrame
	| currentPyFrame newFrames |
	currentPyFrame := topPyFrame.
	newFrames := OrderedCollection new.
	[ currentPyFrame notNone ]
		whileTrue: [
			newFrames add: currentPyFrame.
			currentPyFrame := currentPyFrame f_back ].
	^ newFrames