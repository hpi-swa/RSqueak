as yet unclassified
open
	^ (Smalltalk at: #PythonWorkspace ifAbsent:[self]) new openLabel: 'Python Workspace'
