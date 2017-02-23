as yet unclassified
pythonInitialize
	NextID := 0.
	Python exec: '
import sys
sys.path = ["', Smalltalk image imagePath, '",
	"', Smalltalk image imagePath, '/pypy/lib_pypy",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/lib-tk",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-darwin",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-mac",
	"', Smalltalk image imagePath, '/pypy/lib-python/2.7/plat-mac/lib-scriptpackages",
	"." ]'.
	"Pythonize all Python classes"
	self withAllSubclasses do: [ :ea | ea pythonize ].
	"Install PythonBuiltins (failing atm)"
	"Smalltalk at: #PythonBuiltins put: Python builtins"