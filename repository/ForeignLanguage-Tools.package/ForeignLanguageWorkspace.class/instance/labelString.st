foreign-language-support
labelString
	| breakOnExceptionsText |
	breakOnExceptionsText := self breakOnExceptions
		ifTrue: [ 'break on exceptions' ]
		ifFalse: [ 'no break on exceptions' ].
	^ self languageSymbol asString, ' ',
		self class windowTitlePrefix, ' (', breakOnExceptionsText, ')'
	