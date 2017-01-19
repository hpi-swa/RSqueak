as yet unclassified
printOn: aStream
	self pyFrame
		ifNil: [  aStream nextPutAll: 'Unknown pyFrame' ]
		ifNotNil: [ | lineno name |
			lineno := pyFrame f_lineno.
			name := pyFrame f_code co_filename.
			aStream nextPutAll: lineno asString, ' @ ',  name ].
	