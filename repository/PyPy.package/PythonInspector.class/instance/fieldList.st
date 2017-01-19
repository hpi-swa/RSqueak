as yet unclassified
fieldList
	"Answer the base field list plus an abbreviated list of indices."
	| keys |
	keys := OrderedCollection new.
	keys add: 'self'.
	keys add: 'class'.
	keys addAll: object allAttributes.
	^ keys