overrides
compile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"
	| pySelector compiledMeth methodNode |
	Python vmSpeaksPython ifFalse: [ ^ super compile: code notifying: requestor trailer: bytes ifFail: failBlock ].
	pySelector := PythonCompiler extractPySelector: code.
	compiledMeth := CompiledMethod new methodClass: self; yourself.
	methodNode  := self newCompiler
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	^ CompiledMethodWithNode new method: compiledMeth; node: methodNode.