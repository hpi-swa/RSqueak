as yet unclassified
pythonInitialize
	NextID := 0.
	Python exec: '
import sys
sys.path = [''', Smalltalk image imagePath ,''',
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib_pypy",
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib-python/2.7",
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib-python/2.7/lib-tk",
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib-python/2.7/plat-darwin",
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib-python/2.7/plat-mac",
	"/Users/fniephaus/code/RSqueak/.build/pypy/lib-python/2.7/plat-mac/lib-scriptpackages",
	"." ]'.
	"Pythonize all Python classes"
	self withAllSubclasses do: [ :ea | ea pythonize ].
	"Install PythonBuiltins (failing atm)"
	"Smalltalk at: #PythonBuiltins put: Python builtins"