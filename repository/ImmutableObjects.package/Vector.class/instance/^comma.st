copying
, otherCollection

	| intermediateHolder |
	intermediateHolder := self species
		new: (self size + otherCollection size) 
		streamContents: [:stream |
			stream
				nextPutAll: self;
				nextPutAll: otherCollection].
	^ self class newFrom: intermediateHolder