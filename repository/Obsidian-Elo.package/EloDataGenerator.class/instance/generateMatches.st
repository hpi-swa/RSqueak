instance-generation
generateMatches

	^(0 to: self numMatches) collect: [ :each |	
		self generateMatch: each]