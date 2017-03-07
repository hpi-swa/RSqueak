as yet unclassified
highlight: aText
	^ (Python pygments
		highlight: aText string
		lexer: Python pyLexerPython
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml