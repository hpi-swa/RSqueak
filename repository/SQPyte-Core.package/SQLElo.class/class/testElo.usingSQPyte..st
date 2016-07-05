as yet unclassified
testElo: aNumber usingSQPyte: aBool 

| a time rng count matchCount playerCount |
FileStream startUp: true.
a := SQLDatabase open: ':memory:' usingSQPyte: aBool.

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
count := 0.

time := [

matchCount timesRepeat: [ | rankA rankB winner playerA playerB playerBRow delta outcome expected scoreDiff |
	playerA := (rng next * playerCount) ceiling.
	rankA := (a execute: 'select rank from PLAYERS where playerId=', playerA) next first.
	
	playerBRow := (a execute: 'select playerId, rank from players where abs(rank - ', rankA, ') < 10') next.
	playerB := playerBRow first.
	rankB := playerBRow second.	
	
	scoreDiff := rankA - rankB.
	expected := 1.0 / (1.0 + (10.0 raisedTo: scoreDiff / 40.0)).
	
	outcome := (rng next) < expected ifTrue: [1] ifFalse: [0].
	winner := outcome - 1.
	
	delta := (outcome - expected) * 1.5.
	
	a execute: 'UPDATE players SET rank=', (rankA + delta) , ' WHERE playerID=', playerA, ';'.
	a execute: 'UPDATE players SET rank=', (rankB - delta) , ' WHERE playerID=', playerB, ';'.
	count := count + 1.
	
	a execute: 'INSERT INTO MATCHES values (', playerA, ', ', playerB, ', ', winner, ');'
].

] timeToRun.

FileStream stdout cr;
	nextPutAll: time asString, 'ms'; cr;
	nextPutAll: count asString, ' loops'; cr.
