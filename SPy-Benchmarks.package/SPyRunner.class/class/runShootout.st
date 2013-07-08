benchmarks
runShootout
	"self runShootout explore"
	| stream times |
	stream := ShootoutTests stdout.
	times := Dictionary new.
	{ [ShootoutTests chameneosredux: 2600000 to: stream].
	 } do:	
		[:block | | benchmark t |
		benchmark := (ShootoutTests selectorForSimpleBlock: block) copyUpTo: $:.
		"Smalltalk garbageCollect."
		times at: benchmark put: (t := Time millisecondsToRun: block)].
	^times