overrides
selectedMessage
	"Answer the source code of the currently selected context."
	| aContext |
	aContext := self selectedContext.
	aContext isForeign ifFalse: [ ^ super selectedMessage ].
	^ self class getPySource: aContext