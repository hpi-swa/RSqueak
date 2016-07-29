instance-generation
generatePlayers

	^(0 to: self numPlayers) collect: [ :each |
		self generatePlayer: each]