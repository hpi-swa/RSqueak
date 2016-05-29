as yet unclassified
testElo: aNumber usingSQPyte: aBool 

| a time rng count matchCount playerCount ranks cursor row |
FileStream startUp: true.
a := SQLDatabase open: ':memory:' usingSQPyte: aBool.

time := [

a execute: 'CREATE TABLE IF NOT EXISTS MATCHES (playerIdA integer, playerIdB integer, winnerId integer);'.
a execute: 'CREATE TABLE IF NOT EXISTS PLAYERS (playerId integer, rank integer);'.

matchCount := aNumber.
playerCount := 100.

FileStream stdout nextPut: $p.
(1 to: playerCount) do: [ :idx | 
	a execute: 'INSERT INTO PLAYERS values (', idx, ', 100);'.
].

FileStream stdout nextPut: $m.
rng := Random seed: 1.
matchCount timesRepeat: [ | rndA rndB winner |
	rndA := (rng next * playerCount) ceiling.
	rndB := (rng next * playerCount) ceiling.
	winner := (rng next rounded) + 1.
	a execute: 'INSERT INTO MATCHES values (', rndA, ', ', rndB, ', ', winner, ');'
].

ranks := Array new: playerCount.
(1 to: playerCount) do: [ :idx | ranks at: idx put: 100 ].

cursor := a execute: 'SELECT * FROM MATCHES;'.

count := 0.
[ row := cursor next ] whileNotNil: [
	| playerA playerB scoreDiff expected delta outcome |
	playerA := row first.
	playerB := row second.
	outcome := (row third) - 1.

	scoreDiff := (ranks at: playerA) - (ranks at: playerB).
	expected := 1.0 / (1.0 + (10.0 raisedTo: scoreDiff / 40.0)).
	delta := (outcome - expected) * 1.5.

 	ranks at: playerA put: (ranks at: playerA) + delta.
	ranks at: playerB put: (ranks at: playerB) - delta.
		
	a execute: 'UPDATE players SET rank=', (ranks at: playerA) , ' WHERE playerID=', playerA, ';'.
	a execute: 'UPDATE players SET rank=', (ranks at: playerB) , ' WHERE playerID=', playerB, ';'.
	count := count + 1.
	(count \\ 1000) = 0 ifTrue: [ FileStream stdout nextPut: $.; flush]
]

] timeToRun.

FileStream stdout cr;
	nextPutAll: time asString, 'ms'; cr;
	nextPutAll: count asString, ' loops'; cr.
