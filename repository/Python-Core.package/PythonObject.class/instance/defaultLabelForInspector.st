overrides
defaultLabelForInspector
	self hasClass ifTrue: [ ^ self className, ': ', self asString ].
	^ self asString