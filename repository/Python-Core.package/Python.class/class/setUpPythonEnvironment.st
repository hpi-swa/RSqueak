overrides
setUpPythonEnvironment
	self load: 'sys'.
	self exec: '
import sys
sys.path = ["', Smalltalk image imagePath, '",
	"', Smalltalk image imagePath, '/pypy/lib_pypy",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/lib-tk",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-darwin",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-mac",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-mac/lib-scriptpackages",
	"', Smalltalk image imagePath, '/site-packages",
	"." ]
def _is_expression(x):
	try:
		compile(x, "<string>", "eval")
		return True
	except SyntaxError:
		return False'.
	self load: 're'.
	self setUpPygments