foreign-language-support
labelString
	| title |
	title := self languageSymbol asString, ' ', self class windowTitlePrefix.
	^ self breakOnExceptions
		ifTrue: [ title, ' (break on exceptions)' ]
		ifFalse: [ title ]