converting
reversed

	^ self class newFrom:
		(self species new: self size streamContents: [:aStream|
			self reverseDo: [:each | aStream nextPut: each]])