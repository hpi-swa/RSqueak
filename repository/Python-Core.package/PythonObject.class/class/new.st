overrides
new
	Python vmSpeaksLanguage ifFalse: [ ^ super new ].
	^ self withAll: #()
		