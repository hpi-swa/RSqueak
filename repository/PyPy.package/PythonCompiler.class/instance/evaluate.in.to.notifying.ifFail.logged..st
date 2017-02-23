as yet unclassified
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	| pyCode |
	pyCode := textOrStream contents.
	(receiver isKindOf: Python) ifFalse: [
		pyCode := pyCode copyReplaceAll: 'self' with: receiver pyIdentifier ].
	^ Python run: pyCode