helpers
isCallable: aPyString
	^ (self eval: 'callable(', aPyString ,')') asSmalltalk