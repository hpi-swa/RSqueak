debugging
tempVariableAt: anIndex in: pyFrame
	^ pyFrame f_locals values __call__ at: anIndex