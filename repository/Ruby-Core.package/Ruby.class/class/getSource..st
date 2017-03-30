source code
getSource: rbFrame
	| filename |
	rbFrame has_contents asSmalltalk ifFalse: [ ^ 'no contents' ].
	filename := rbFrame get_filename asSmalltalk.
	^ (FileStream isAFileNamed: filename)
		ifTrue: [ self getContentsOf: filename ]
		ifFalse: [ rbFrame get_code_source asSmalltalk ]