overrides
setUpPygments
	self exec: '
try:
	import pygments
	import pygments.lexers
	import pygments.formatters
	_pygments_lexer_python = pygments.lexers.get_lexer_by_name(
		"python", encoding="utf-8", stripnl=False, ensurenl=False)
	_pygments_lexer_ruby = pygments.lexers.get_lexer_by_name(
		"ruby", encoding="utf-8", stripnl=False, ensurenl=False)
	_pygments_lexer_smalltalk = pygments.lexers.get_lexer_by_name(
		"smalltalk", encoding="utf-8", stripnl=False, ensurenl=False)
	_pygments_formatter = pygments.formatters.get_formatter_by_name(
		"html", encoding="utf-8", style="colorful", nowrap=True, noclasses=True, lineseparator="<br>")
except ImportError:
	pass
' breakOnExceptions: false.