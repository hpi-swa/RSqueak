helpers
findResumeFrame: aContext
	| currentCtx |
	currentCtx := aContext.
	[ currentCtx notNil ] whileTrue: [
		(currentCtx method selector = #vmEval "special selector"
			and: [ currentCtx closure isNil ])
				ifTrue: [ ^ currentCtx ].
		currentCtx := currentCtx sender ].
	self error: 'resume context not found'