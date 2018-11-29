helpers
prependContexts: resumeContext
	| topForeignFrame newFrames |
	[ topForeignFrame := self primTopFrame ]
		on: Error do: [ ^ resumeContext ].
	newFrames := ((self getForeignFrames: topForeignFrame)
		collect: [ :ea | self newForeignContextFor: ea ])
			overlappingPairsDo: [ :a :b | a sender: b ].
	newFrames last sender: resumeContext.
	^ newFrames first