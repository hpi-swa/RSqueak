*SPy-Benchmarks
run: aResult 
	self tests do: [:each | 
		self changed: each.
		SPyVM print: 'Running Test ', (each asString padded: #right to: 62 with: $ ), String tab, '(', aResult asString, ')'.
		each run: aResult].