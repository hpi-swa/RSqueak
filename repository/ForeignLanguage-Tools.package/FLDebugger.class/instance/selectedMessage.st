overrides
selectedMessage
	"Answer the source code of the currently selected context."
	| aContext |
	aContext := self selectedContext.
	aContext isForeign ifFalse: [ ^ super selectedMessage ].
	Smalltalk at: aContext languageSymbol ifPresent: [ :cls |
		^ cls getSource: aContext foreignFrame ]