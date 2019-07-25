testing
innerPrintListOn: aStream

	self car printOn: aStream.
	VNil nil = self cdr
		ifFalse: [self cdr isCons
			ifFalse: [
				aStream nextPutAll: ' . '.
				self cdr printOn: aStream]
			ifTrue: [
				aStream space.
				self cdr innerPrintListOn: aStream]]
