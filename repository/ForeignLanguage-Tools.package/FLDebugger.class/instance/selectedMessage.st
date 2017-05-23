overrides
selectedMessage
	| aContext |
	aContext := self selectedContext.
	aContext isForeign ifFalse: [ ^ super selectedMessage ].
	^ aContext sourceCode