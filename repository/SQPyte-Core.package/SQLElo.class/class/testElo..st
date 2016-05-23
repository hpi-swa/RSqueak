as yet unclassified
testElo: aDriver

| a rng matchCount playerCount ranks cursor row |
FileStream startUp: true.
a := SQLDatabase open: ':memory:' with: aDriver.
a execute: 'CREATE TABLE IF NOT EXISTS MATCHES (playerIdA integer, playerIdB integer, winnerId integer);'.
a execute: 'CREATE TABLE IF NOT EXISTS PLAYERS (playerId integer, rank integer);'.

matchCount := 10000.
playerCount := 100.

FileStream stdout nextPut: $p.
(1 to: playerCount) do: [ :idx | 
	a execute: 'INSERT INTO PLAYERS values (?, 100);' args: {idx}.
].

FileStream stdout nextPut: $m.
rng := Random new.
matchCount timesRepeat: [ | rndA rndB winner |
	rndA := (rng next * playerCount) ceiling.
	rndB := (rng next * playerCount) ceiling.
	winner := 2 atRandom.
	a execute: 'INSERT INTO MATCHES values (?, ?, ?);' args: {rndA. rndB. winner}
].

a execute: 'SELECT * FROM MATCHES;'.
ranks := Array new: playerCount.
(1 to: playerCount) do: [ :idx | ranks at: idx put: 100 ].

cursor := a execute: 'SELECT * FROM MATCHES;'.


[ row := cursor next ] whileNotNil: [
	| playerA playerB scoreDiff expected delta outcome |
	playerA := row first asInteger.
	playerB := row second asInteger.
	outcome := (row third asInteger) - 1.

	scoreDiff := (ranks at: playerA) - (ranks at: playerB).
	expected := 1.0 / (1.0 + (10.0 raisedTo: scoreDiff / 40.0)).
	delta := (outcome - expected) * 1.5.

 	ranks at: playerA put: (ranks at: playerA) + delta.
	ranks at: playerB put: (ranks at: playerB) - delta.
		
	a execute: 'UPDATE players SET rank=', (ranks at: playerA), ' WHERE playerID=', playerA, ';'.
	a execute: 'UPDATE players SET rank=', (ranks at: playerB), ' WHERE playerID=', playerB, ';'.
].

^ a execute: 'select * from players'.