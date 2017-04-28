as yet unclassified
highlightPython: aText
	^ (Python pygmentsHighlight
		__call__: aText string
		lexer: Python pyLexerPython
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml