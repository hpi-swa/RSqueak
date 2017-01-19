overrides
getPySource: aContext
	| filename |
	aContext isPyContext ifFalse: [ 1 halt. ^ 'Not a Python context!!!' ].
	filename := aContext pyFrame f_code co_filename.
	filename = '<string>' ifTrue: [ ^ self getPySourceString: aContext ].
	^ self getPySourceContent: filename
	