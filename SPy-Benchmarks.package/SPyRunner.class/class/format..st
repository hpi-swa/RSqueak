benchmarks
format: result
	"self run"
	"runs all the benchmarks and creates a string of csv format with one column per benchmarks"
	^ String streamContents: 
		[ :aStream | | someSymbols |
		someSymbols := result keys.
		someSymbols 
			do: [ :aSymbol | aStream << aSymbol]
			separatedBy: [ aStream << ';' ].
		aStream << String lf.
		someSymbols 
			do: [ :aSymbol | aStream << (result at: aSymbol)]
			separatedBy: [aStream << ';']]