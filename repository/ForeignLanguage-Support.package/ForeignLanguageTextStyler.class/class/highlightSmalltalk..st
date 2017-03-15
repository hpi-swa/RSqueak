as yet unclassified
highlightSmalltalk: aText
	^ (Python pygments
		highlight: aText string
		lexer: Python pyLexerSmalltalk
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml