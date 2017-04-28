overrides
process: aProcess controller: aController context: aContext
	| ctx |
	(aContext method selector = #vmResume: and: [
		aContext receiver inheritsFrom: ForeignLanguage ])
			ifFalse: [ ^ super process: aProcess controller: aController context: aContext].
	ctx := aContext receiver prependContexts: aContext languageProcess: (aContext at: 1).
	super process: aProcess controller: aController context: ctx