set up
fillArrayOfSize: aNumber with: aBlock

	^ Array new: aNumber streamContents: [:strm |
		aNumber timesRepeat: [strm nextPut: aBlock value]]