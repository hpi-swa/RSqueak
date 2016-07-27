as yet unclassified
testSQLite

| a rng matchCount playerCount ranks |
a := SQLiteConnection new.
a fileName: ':memory:'.
a open.
a executeQuery: 'CREATE TABLE IF NOT EXISTS MATCHES (playerIdA integer, playerIdB integer, winnerId integer);'.

a executeQuery: 'CREATE TABLE IF NOT EXISTS PLAYERS (playerId integer, rank integer);'.

matchCount := 10000.
playerCount := 100.

(1 to: playerCount) do: [ :idx |
	a executeQuery: 'insert into players values (', idx, ', 100)'.
].

rng := Random new.
matchCount timesRepeat: [ | rndA rndB winner |
	rndA := (rng next * playerCount) ceiling.
	rndB := (rng next * playerCount) ceiling.
	winner := 2 atRandom.
	a executeQuery: 'INSERT INTO MATCHES values (' , rndA , ', ' , rndB , ', ' , winner , ');'.
].

a executeQuery: 'SELECT * FROM MATCHES;'.
ranks := Array new: playerCount.
(1 to: playerCount) do: [ :idx | ranks at: idx put: 100 ].

a withResultForQuery: 'SELECT * FROM MATCHES;' do: [:res |
	Transcript show: 'result:'.
	res do: [:row |
		| playerA playerB scoreDiff expected delta outcome |
		playerA := (row at: 'playerIdA') asInteger.
    		playerB := (row at: 'playerIdB') asInteger.
		outcome := ((row at: 'winnerId') asInteger) - 1.
		scoreDiff := (ranks at: playerA) - (ranks at: playerB).
		expected := 1.0 / (1.0 + (10.0 raisedTo: scoreDiff / 40.0)).
		delta := (outcome - expected) * 1.5.

	 	ranks at: playerA put: (ranks at: playerA) + delta.
		ranks at: playerB put: (ranks at: playerB) - delta.
		
		a executeQuery: 'UPDATE players SET rank=', (ranks at: playerA), ' WHERE playerID=', playerA, ';'.
		a executeQuery: 'UPDATE players SET rank=', (ranks at: playerB), ' WHERE playerID=', playerB, ';'.
	].
].

^ a executeQuery: 'select * from players'.