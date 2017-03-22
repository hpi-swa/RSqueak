source code
getSource: rbFrame
	| filename |
	rbFrame has_contents asSmalltalk ifFalse: [ ^ 'no contents' ].
	filename := rbFrame get_filename asSmalltalk.
	filename = '-e' ifTrue: [ ^ 'source unavailable (-e)' ].
	^ self getContentsOf: filename