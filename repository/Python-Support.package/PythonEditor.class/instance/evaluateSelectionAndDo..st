as yet unclassified
evaluateSelectionAndDo: aBlock
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ nil].

	(model respondsTo: #evaluateExpression:) ifTrue: [
		^ aBlock value: (model perform: #evaluateExpression: with: self selection)].

	(model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := model doItReceiver.
				ctxt := model doItContext]
		ifFalse: [rcvr := ctxt := nil].
	result := [
		PythonCompiler new 
			evaluate: self selectionAsStream
			in: ctxt
			to: rcvr
			environment: (model environment ifNil: [Smalltalk globals])
			notifying: self
			ifFail: [morph flash. ^ nil]
			logged: true.
	] 
		on: OutOfScopeNotification 
		do: [ :ex | ex resume: true].
		
	(model respondsTo: #expressionEvaluated:result:) ifTrue: [
		model perform: #expressionEvaluated:result: with: self selection with: result].
		
	^aBlock value: result