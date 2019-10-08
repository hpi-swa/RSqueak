*Values-lambcompare
valuesBenchAppendn

	| e lst1 lst2 res start end gcAfter gcBefore |
	e := 17.
	lst1 := VCons withAll: (Array new: self withAll: e).
	lst2 := VCons withAll: (Array new: self withAll: e).
	Smalltalk garbageCollect.
	gcBefore := RSqueak gcTimings.
	start := Time utcMicrosecondClock.
	res := lst1 append: lst2.
	end := Time utcMicrosecondClock.
	gcAfter := RSqueak gcTimings.
	RSqueak reportBenchStartedAt: start until: end gcBefore: gcBefore after: gcAfter.
	self assert: (res isKindOf: VNil) not.
	^ 0