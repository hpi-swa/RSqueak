sql
benchmark: aSql times: aNumber times: anotherNumber
	anotherNumber timesRepeat: [
		Transcript show: (self benchmark: aSql times: aNumber); cr ]