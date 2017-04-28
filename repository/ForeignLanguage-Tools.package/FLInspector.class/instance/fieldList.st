overrides
fieldList
	"Answer the base field list plus an abbreviated list of indices."
	| keys |
	keys := OrderedCollection new.
	keys add: 'self'.
	object isForeign ifTrue: [ keys addAll: object allInstVarNames ].
	^ keys