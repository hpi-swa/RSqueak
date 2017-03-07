overrides
contextForStack: aContext
	aContext method selector = #resumeFrame
		ifFalse: [ ^ aContext ]. "Normal MethodContext"
	^ self class prependPythonContexts: aContext