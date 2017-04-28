as yet unclassified
highlightRuby: aText
	Python vmSpeaksLanguage ifFalse: [ ^ aText ].
	^ (Python pygmentsHighlight
		__call__: aText string
		lexer: Python pyLexerRuby
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml