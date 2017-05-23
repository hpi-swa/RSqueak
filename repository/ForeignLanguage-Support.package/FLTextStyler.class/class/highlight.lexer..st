as yet unclassified
highlight: aText lexer: aLexer
	Python vmSpeaksLanguage ifFalse: [ ^ aText ].
	^ (Python pygmentsHighlight
		__call__: aText string
		lexer: aLexer
		formatter: Python htmlFormatter) asSmalltalk asTextFromHtml