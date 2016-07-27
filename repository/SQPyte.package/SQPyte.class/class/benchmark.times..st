sql
benchmark: aSql times: aNumber
	^ [ aNumber timesRepeat: [ self run: aSql ] ] timeToRun