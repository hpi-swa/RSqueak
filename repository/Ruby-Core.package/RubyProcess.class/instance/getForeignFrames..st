helpers
getForeignFrames: topRbFrame
	| currentRbFrame newFrames |
	currentRbFrame := topRbFrame.
	newFrames := OrderedCollection new.
	[ currentRbFrame notNil ]
		whileTrue: [
			newFrames add: currentRbFrame.
			currentRbFrame := currentRbFrame get_previous ].
	^ newFrames