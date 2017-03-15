helpers
availableLanguages
	^ (self subclasses collect: [ :ea | ea asString ]) sorted
		inject: (OrderedCollection with: #Smalltalk)
		into: [ :coll :ea | coll add: ea asSymbol; yourself ]