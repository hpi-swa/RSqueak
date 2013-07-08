*SPy-Benchmarks
run: aResult 
	self tests do: [:each | 
		self changed: each.
		SPyVM print: 'Running Test ', each asString.
		each run: aResult].