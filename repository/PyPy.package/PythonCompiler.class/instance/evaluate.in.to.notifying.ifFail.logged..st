as yet unclassified
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	| pyCode |
	pyCode := textOrStream contents.
	(receiver isKindOf: Python) ifFalse: [
		pyCode := pyCode copyReplaceAll: 'self' with: receiver pyIdentifier ].
	(Python evaluable: pyCode)
		ifTrue: [ [^ Python eval: pyCode ] on: Error do: [] ].
	^ Python exec: pyCode