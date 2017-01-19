as yet unclassified
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	| pyCode |
	pyCode := textOrStream contents.
	(receiver isKindOf: Python) ifFalse: [
		pyCode := pyCode copyReplaceAll: 'self' with: receiver pyIdentifier ].
	"= or multilines cannot be evaluated, but always need to be executed instead"
	(pyCode includesAnyOf: {$=. String cr}) ifTrue: [
		[^ Python eval: pyCode ] on: Error do: []].
	^ Python exec: pyCode
