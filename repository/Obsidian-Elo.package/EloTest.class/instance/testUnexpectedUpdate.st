testing
testUnexpectedUpdate
	
	| black white match |
	black := EloPlayer new score: 110.
	white := EloPlayer new score: 90.
	match := (EloMatch new) blackPlayer: black; whitePlayer: white; outcome: 1.
	
	match adjustPlayerScores.
	"in a symmetric case, winner gains (EloMatch maxMovedPoints / 2) points,
	here we expect less, because black is the stronger player and a win
	less surprising"
	self assert: black score < (110 + (EloMatch maxMovedPoints / 2)).
	self assert: black score > (90 + (EloMatch maxMovedPoints / 2)).