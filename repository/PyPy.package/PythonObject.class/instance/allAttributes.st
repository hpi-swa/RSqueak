helpers
allAttributes
	| attributes |
	attributes := OrderedCollection new.
	(self hasAttribute: '__dict__') ifTrue: [ attributes add: '__dict__'].
	attributes addAll: self pyDictKeys.
	^ attributes