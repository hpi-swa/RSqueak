instance creation
withAll: aCollection

"	^ aCollection
		ifEmpty: [nil]
		ifNotEmpty: [:collection |
			self car: collection first cdr: (self withAll: collection allButFirst)]
"
	| list |
	list := nil.
	aCollection reverseDo: [:each | list := Cons car: each cdr: list].
	^ list
