*SQPyte-Core
testPreparedEloSQPyte: aString
	^ SQLElo testPreparedElo: SQPyteDriver match: aString asInteger.
	