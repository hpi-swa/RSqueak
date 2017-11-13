overrides
startUp: resuming
	(resuming and: [ self vmSpeaksLanguage ])
		ifTrue: [ Ruby eval: 'def blank_binding; binding; end' ]