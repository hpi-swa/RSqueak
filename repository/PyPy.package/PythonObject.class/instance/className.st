helpers
className
	self hasClass ifFalse: [ self error: 'No class found' ].
	^ self __class__ __name__ asSmalltalk