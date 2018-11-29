helpers
findResumeContext: aContext
	| currentCtx |
	currentCtx := aContext.
	[ currentCtx notNil ] whileTrue: [
		(currentCtx method selector = #vmResume "special selector"
			and: [ currentCtx closure isNil ])
				ifTrue: [ ^ currentCtx ].
		currentCtx := currentCtx sender ].
	self error: 'resume context not found'