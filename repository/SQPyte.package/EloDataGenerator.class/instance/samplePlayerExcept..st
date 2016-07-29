random-sampling
samplePlayerExcept: playerId
	"Sample a player ID except the given one"
	
	| id |
	id := playerId.
	[id = playerId] whileTrue: [id := self samplePlayerId].
	^id