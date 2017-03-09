as yet unclassified
highlight: aText
	^ (Python eval: '_pygments_highlight("""', aText string, '""")') asSmalltalk asTextFromHtml
	"^ (Python pygments
		highlight: aText string
		lexer: Python pyLexerPython
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml"