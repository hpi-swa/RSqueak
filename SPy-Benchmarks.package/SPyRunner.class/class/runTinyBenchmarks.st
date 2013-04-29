benchmarks
runTinyBenchmarks
	| t1 t2 r n1 n2 |
	n1 := 5200.
	t1 := Time millisecondsToRun: [n1 benchmark].
	"Note: #benchmark's runtime is about O(n)"

	n2 := 42.
	t2 := Time millisecondsToRun: [r := n2 benchFib].
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^ Dictionary new 
		at: #benchmark put: t1;
		at: #benchFib put: t2;
		yourself "((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printString, ' sends/sec'"