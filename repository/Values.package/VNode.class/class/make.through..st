instance creation
make: anObject through: aNumber

	^ aNumber <= 0
		ifTrue: [VLeaf val: anObject]
		ifFalse: 
			[ | down |
			down := aNumber - 1.
			self
				left: (self make: anObject through: down)
				val: anObject
				right: (self make: anObject through: down)]
		