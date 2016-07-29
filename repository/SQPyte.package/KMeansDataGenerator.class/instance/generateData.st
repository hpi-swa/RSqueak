as yet unclassified
generateData
	
	^ (1 to: self numDataPoints) collect: [ :each |	
		self generateDataPoint].