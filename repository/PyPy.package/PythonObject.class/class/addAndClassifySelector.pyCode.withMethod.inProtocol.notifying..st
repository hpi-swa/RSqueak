as yet unclassified
addAndClassifySelector: selector pyCode: aPyCode withMethod: compiledMethod inProtocol: category notifying: requestor
	| priorMethodOrNil |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: [nil].
	self codeAt: selector put: aPyCode.
	SystemChangeNotifier uniqueInstance 
		doSilently: [self organization classify: selector under: category].
	priorMethodOrNil isNil
		ifTrue: [SystemChangeNotifier uniqueInstance methodAdded: compiledMethod selector: selector inProtocol: category class: self requestor: requestor]
		ifFalse: [SystemChangeNotifier uniqueInstance methodChangedFrom: priorMethodOrNil to: compiledMethod selector: selector inClass: self requestor: requestor].