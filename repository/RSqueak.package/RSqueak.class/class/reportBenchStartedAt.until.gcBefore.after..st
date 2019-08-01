as yet unclassified
reportBenchStartedAt: start until: end gcBefore: gcBefore after: gcAfter
	
	| gcTaken millisecondsTaken |
	millisecondsTaken := (end - start) * 0.001.
	gcTaken := (gcAfter - gcBefore) asFloat.
	RSqueak
		debugPrint: ('0:RESULT-cpu:ms: ', millisecondsTaken);
		debugPrint: ('0:RESULT-total:ms: ', millisecondsTaken);
		debugPrint: ('0:RESULT-gc:ms: ', gcTaken);
		yourself.