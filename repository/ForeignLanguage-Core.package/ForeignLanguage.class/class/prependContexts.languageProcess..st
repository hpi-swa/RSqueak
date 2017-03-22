debugging
prependContexts: resumeContext languageProcess: languageProcess
	| topForeignFrame newFrames |
	[ topForeignFrame := self primGetTopFrame: languageProcess ]
		on: Error do: [ ^ resumeContext ].
	newFrames := ((self getForeignFrames: topForeignFrame)
		collect: [ :ea | self newForeignContextFor: ea ])
			overlappingPairsDo: [ :a :b | a sender: b ].
	newFrames last sender: resumeContext.
	^ newFrames first