helpers
load: aModuleName
	self import: aModuleName.
	^ Python eval: aModuleName