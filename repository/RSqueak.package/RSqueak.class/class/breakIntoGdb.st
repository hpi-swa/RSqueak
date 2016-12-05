VMDebugging-primitives
breakIntoGdb

	Smalltalk platformName = 'Win32'
		ifTrue: [self error: 'Break into gdb not implemented for Windows'].
	self privBreakIntoGdb.