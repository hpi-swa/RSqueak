overrides
toolIconSelector: aSelector
	(self isMeta or: [(self pyMethodDict includesKey: aSelector) not])
		ifTrue: [^ super toolIconSelector: aSelector].
	^#python