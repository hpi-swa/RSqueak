overrides
messageIconAt: anIndex
	| context |
	Browser showMessageIcons
		ifFalse: [^ nil].
	context := contextStack at: anIndex.
	^ context isForeign
		ifTrue: [ ToolIcons iconNamed: context languageSymbol ]
		ifFalse: [ super messageIconAt: anIndex ]