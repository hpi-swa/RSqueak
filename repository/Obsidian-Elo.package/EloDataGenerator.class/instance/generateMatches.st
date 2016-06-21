instance-generation
generateMatches

	^(1 to: self numMatches) collect: [ :each |	
		self generateMatch: each]