helpers
load: aModuleName breakOnExceptions: aBool
	self import: aModuleName breakOnExceptions: aBool.
	^ self fromObjectCache: aModuleName