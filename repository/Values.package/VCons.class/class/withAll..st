instance creation
withAll: aCollection

"	^ aCollection
		ifEmpty: [VNil nil]
		ifNotEmpty: [:collection |
			self car: collection first cdr: (self withAll: collection allButFirst)]
"
	| list |
	list := VNil nil.
	aCollection reverseDo: [:each | list := self car: each cdr: list].
	^ list
