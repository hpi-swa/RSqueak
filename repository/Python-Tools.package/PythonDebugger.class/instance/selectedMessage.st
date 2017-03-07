overrides
selectedMessage
	"Answer the source code of the currently selected context."
	| aContext |
	aContext := self selectedContext.
	aContext isPython ifFalse: [ ^ super selectedMessage ].
	^ self class getPySource: aContext