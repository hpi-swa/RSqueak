instance-generation
generatePlayers

	^(1 to: self numPlayers) collect: [ :each |
		self generatePlayer: each]