overrides
new
	Python vmSpeaksPython ifFalse: [ ^ super new ].
	^ self withAll: #()
		