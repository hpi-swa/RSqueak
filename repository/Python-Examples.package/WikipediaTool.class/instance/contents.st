as yet unclassified
contents
	self searchTerm ifNil: [^''].
	Python exec: 'import wikipedia'.
	^ ((Python eval: 'wikipedia.summary') __call__: self searchTerm asString) asSmalltalk