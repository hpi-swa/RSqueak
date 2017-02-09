copying
copyFrom: start to: stop

	| temporaryHolder |
	stop < start ifTrue: [^ self copyEmpty].

	temporaryHolder := self species
		new: stop - start + 1
		streamContents: [:aStream |
			start to: stop do: [:index |
				aStream nextPut: (self at: index)]].
	^ self class newFrom: temporaryHolder