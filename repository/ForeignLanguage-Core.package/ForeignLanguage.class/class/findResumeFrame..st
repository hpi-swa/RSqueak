debugging
findResumeFrame: aContext
	| currentCtx |
	currentCtx := aContext.
	[ currentCtx notNil ] whileTrue: [
		(currentCtx method selector = #resume:
			and: [ currentCtx closure isNil ])
				ifTrue: [ ^ currentCtx ].
		currentCtx := currentCtx sender ].
	self error: 'resume context not found'