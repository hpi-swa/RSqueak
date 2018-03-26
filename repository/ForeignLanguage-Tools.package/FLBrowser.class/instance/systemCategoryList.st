overrides
systemCategoryList
	"Answer the class categories modelled by the receiver."
	self languageSymbol = #Smalltalk ifTrue: [ ^ super systemCategoryList ].
	^ (super systemCategoryList select: [:ea |
		(ea beginsWith: 'ForeignLanguage') or: [ ea beginsWith: self languageSymbol asString ]]) sorted