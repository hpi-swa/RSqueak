overrides
style: aText foreignClass: foreignClass
	| styledText |
	stylingEnabled ifFalse: [ ^ self ].
	foreignClass vmSpeaksLanguage ifFalse: [^ super style: aText].
	styledText := (foreignClass highlight: aText) first: aText size.
	"Strings must be of same size, otherwise image might crash"
	self assert: styledText string size = aText string size.
	view ifNotNil: [ view stylerStyled: styledText ]