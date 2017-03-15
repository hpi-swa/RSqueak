as yet unclassified
highlightRuby: aText
	Python vmSpeaksLanguage ifFalse: [ ^ aText ].
	^ (Python eval: '_pygments_highlight("""', aText string, '""", _pygments_lexer_ruby)') asSmalltalk asTextFromHtml
	"^ (Python pygments
		highlight: aText string
		lexer: Python pyLexerPython
		formatter: Python pyFormatter) asSmalltalk asTextFromHtml"