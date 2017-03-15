debugging
debug: aProcess context: aContext label: aString contents: contents fullView: aBool
	"Open a debugger on the given process and context."
	^ ForeignLanguage openOn: aProcess context: aContext label: aString contents: contents fullView: aBool