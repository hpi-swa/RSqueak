overrides
contents: aText notifying: aController
	| ctx |
	ctx := self selectedContext.
	ctx isForeign ifFalse: [^ super contents: aText notifying: aController].
	Smalltalk at: ctx languageSymbol ifPresent: [ :cls |
		cls setSource: ctx foreignFrame to: aText asString.
		cls restartFrame: ctx foreignFrame with: aText asString withUnixLineEndings ].
	contents := aText.
	^ true