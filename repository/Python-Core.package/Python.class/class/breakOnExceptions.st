helpers
breakOnExceptions
	^ BreakOnExceptions ifNil: [ BreakOnExceptions := true ]