overrides
messageIconAt: anIndex
	| context |
	Browser showMessageIcons
		ifFalse: [^ nil].
	context := contextStack at: anIndex.
	context isPython
		ifTrue: [ ^ ToolIcons iconNamed: #python ]
		ifFalse: [ ^ super messageIconAt: anIndex ]