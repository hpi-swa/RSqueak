overrides
contextForStack: aContext
	(aContext method literalAt: 3) = #primResume
		ifFalse: [ ^ aContext ]. "Normal MethodContext"
	^ self appendPythonContexts: aContext