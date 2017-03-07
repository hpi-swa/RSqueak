as yet unclassified
debugContext: aContext label: aString contents: contents
	"Open a debugger on the given process and context."
	^ PythonDebugger openContext: aContext label: aString contents: contents