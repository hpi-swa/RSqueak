convenience
treeWithMin: aNumber max: anotherNumber element: item

	| maxDepth stretchDepth ignore longLived minDepth |
	minDepth := aNumber.
	maxDepth := anotherNumber.
	stretchDepth := anotherNumber + 1.
	ignore := self make: item through: stretchDepth.
	longLived := self make: item through: maxDepth.

	minDepth to: maxDepth do:
		[:d | |iterations|
		iterations := 2 raisedTo: (maxDepth - d).
		iterations timesRepeat:
			[ | ignore1 ignore2 |
			ignore1 := self make: item through: d.
			ignore2 := self make: item through: d]].

	^ longLived check