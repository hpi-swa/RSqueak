VMDebugging-primitives
debugPrint: aString

	<primitive: 'debugPrint' module: 'VMDebugging'>
	FileStream stdout nextPutAll: aString asString; cr.