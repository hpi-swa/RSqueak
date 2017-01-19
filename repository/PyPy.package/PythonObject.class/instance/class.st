overrides
class
	self isClass
		ifTrue: [ ^ self ]
		ifFalse: [ ^ self __class__ ]