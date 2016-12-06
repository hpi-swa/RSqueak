as yet unclassified
withAll: aCollection

	^ aCollection
		ifEmpty: [nil]
		ifNotEmpty: [:collection |
			self car: collection first cdr: (self withAll: collection allButFirst)]