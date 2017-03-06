testing
innerPrintListOn: aStream

	self car printOn: aStream.
	self cdr
		ifNotNil: [:cont | cont isCons
			ifFalse: [
				aStream nextPutAll: ' . '.
				cont printOn: aStream]
			ifTrue: [
				aStream space.
				cont innerPrintListOn: aStream]]
