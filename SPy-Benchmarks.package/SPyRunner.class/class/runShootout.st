benchmarks
runShootout
	"self runShootout explore"
	| stream times |
	stream := (ByteString new: 10000) writeStream.
	times := Dictionary new.
	{ [ShootoutTests nbody: 200000 "20000000" to: stream].
	   [ShootoutTests binarytrees: 17 to: stream].
	   "[ShootoutTests chameneosredux: 2600000 to: stream].
	   [ShootoutTests threadring: 100000000 to: stream]" } do:	
		[:block | | benchmark t |
		benchmark := (ShootoutTests selectorForSimpleBlock: block) copyUpTo: $:.
		"Smalltalk garbageCollect."
		times at: benchmark put: (t := Time millisecondsToRun: block)].
	^times