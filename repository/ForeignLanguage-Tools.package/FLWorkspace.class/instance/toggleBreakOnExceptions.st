accessing
toggleBreakOnExceptions
	self breakOnExceptions: self breakOnExceptions not.
	self changed: #relabel