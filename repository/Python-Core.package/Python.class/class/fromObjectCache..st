helpers
fromObjectCache: aName
	ObjectCache ifNil: [ ObjectCache := Dictionary new ].
	^ ObjectCache at: aName ifAbsentPut: [ self eval: aName ]