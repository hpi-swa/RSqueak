as yet unclassified
pythonInitialize
	NextID := 0.
	Python exec: '
import sys
sys.path.append(''', Smalltalk image imagePath ,''')'.
	"Pythonize all Python classes"
	self withAllSubclasses do: [ :ea | ea pythonize ].
	"Install PythonBuiltins (failing atm)"
	"Smalltalk at: #PythonBuiltins put: Python builtins"