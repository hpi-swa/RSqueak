overrides
defaultLabelForInspector
	"Answer the default label to be used for an Inspector window on the receiver."
	self hasClass ifTrue: [ ^ self className, ': ', self asString ].
	^ self asString