debugging
debugContext: aContext label: aString contents: contents
	"Open a debugger on the given process and context."
	^ ForeignLanguage openContext: aContext label: aString contents: contents