accessing
testSelectors
	" Ensure those tests are not run on a 'normal' VM "

	^ self immutabilityAvailable
		ifFalse: [#()]
		ifTrue: [super testSelectors]