overrides
contents: aText notifying: aController
	| ctx |
	ctx := self selectedContext.
	ctx isPyContext ifFalse: [^ super contents: aText notifying: aController].
	self class setPySourceContent: ctx pyFrame f_code content: aText asString.
	Python restartFrame: ctx pyFrame with: aText asString withUnixLineEndings.
	contents := aText.
	^ true