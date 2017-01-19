overrides
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource 
	| methodAndNode selector |
	methodAndNode := self
		compile: text asString
		notifying: requestor
		trailer: (self defaultMethodTrailerIfLogSource: logSource)
		ifFail: [ ^ nil ].
	selector := methodAndNode selector.
	(Python vmSpeaksPython not and: [logSource]) ifTrue:
		[ self
			logMethodSource: text
			forMethodWithNode: methodAndNode
			inCategory: category
			withStamp: changeStamp
			notifying: requestor.
		RecentMessages default
			recordSelector: selector
			forClass: methodAndNode method methodClass
			inEnvironment: CurrentEnvironment signal ].
	Python vmSpeaksPython
		ifTrue: [
			self
				addAndClassifySelector: selector
				pyCode: text
				withMethod: methodAndNode method
				inProtocol: category
				notifying: requestor ]
		ifFalse:  [
			self
				addAndClassifySelector: selector
				withMethod: methodAndNode method
				inProtocol: category
				notifying: requestor ].
	self instanceSide
		noteCompilationOf: selector
		meta: self isClassSide.
	^ selector