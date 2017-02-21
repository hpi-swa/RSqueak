overrides
selectedMessage
	"Answer the source code of the currently selected context."
	| aContext |
	aContext := self selectedContext.
	aContext isPyContext ifFalse: [ ^ super selectedMessage ].
	^ self class getPySource: aContext