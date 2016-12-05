VMDebugging-primitives
perform: aSelector with: anArrayOfArguments
	"Run some code and return the result, without propagating any exceptions to Smalltalk or even using the Smalltalk stack"
	<primitive: 'performHeadless' module: 'VMDebugging'>
	self primitiveFailed.