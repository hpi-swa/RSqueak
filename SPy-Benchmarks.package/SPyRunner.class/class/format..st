benchmarks
format: result
	"self run"
	"runs all the benchmarks and creates a string of csv format with one column per benchmarks"
	^ String streamContents: 
		[ :aStream |
		result keysAndValuesDo: 
			[ :key :value | 
			aStream << key << ';' << value << String lf]]