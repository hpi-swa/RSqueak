utilities
alternatingArrayOfSize: aNumber with: anObject andAlternate: anotherObject

	^ Array new: aNumber streamContents:
		[:strm| 1 to: aNumber do: 
			[:num | strm nextPut: (num even ifTrue: [anObject] ifFalse: [anotherObject])]]