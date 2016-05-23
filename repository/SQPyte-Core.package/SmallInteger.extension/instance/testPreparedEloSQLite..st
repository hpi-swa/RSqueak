*SQPyte-Core
testPreparedEloSQLite: aString
	^ SQLElo testPreparedElo: SQLiteDriver match: aString asInteger.
	