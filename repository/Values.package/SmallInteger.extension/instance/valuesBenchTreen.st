*Values-lambcompare
valuesBenchTreen

	|  end gcAfter gcBefore item minDepth num res start |
	minDepth := 3.
	num := (minDepth - 1) + (self / 1000000).
	RSqueak debugPrint: 'num: ', num asString.
	item := 17.
	Smalltalk garbageCollect.
	gcBefore := RSqueak gcTimings.
	start := Time utcMicrosecondClock.
	res := VNode treeWithMin: minDepth max: num element: item.
	end := Time utcMicrosecondClock.
	gcAfter := RSqueak gcTimings.
	RSqueak reportBenchStartedAt: start until: end gcBefore: gcBefore after: gcAfter.
	self assert: res = item.
	^ 0