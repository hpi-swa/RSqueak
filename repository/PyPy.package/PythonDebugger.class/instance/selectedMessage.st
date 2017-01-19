overrides
selectedMessage
	"Answer the source code of the currently selected context."
	| aContext |
	aContext := self selectedContext.
	aContext isPyContext ifTrue: [ ^ self getPySource: aContext ].
	^contents := aContext debuggerMap sourceText asText makeSelectorBoldIn: aContext home receiver class