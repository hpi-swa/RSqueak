overrides
process: aProcess controller: aController context: aContext
	| ctx |
	(aContext method selector = #vmResume)
			ifFalse: [ ^ super process: aProcess controller: aController context: aContext].
	ctx := aContext receiver prependContexts: aContext.
	^ super process: aProcess controller: aController context: ctx