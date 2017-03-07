overrides
stylePython: aText
	| styledText |
	stylingEnabled ifFalse: [ ^ self ].
	Python vmSpeaksPython ifFalse: [^ super style: aText].
	styledText := (PythonTextStyler highlight: aText) first: aText size.
	"Strings must be of same size, otherwise image might crash"
	self assert: styledText string size = aText string size.
	view ifNotNil:[view stylerStyled: styledText]