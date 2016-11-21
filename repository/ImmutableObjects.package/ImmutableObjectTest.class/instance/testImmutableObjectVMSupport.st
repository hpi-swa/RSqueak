testing
testImmutableObjectVMSupport
	self
		assert: (Object immutableNew commit) isImmutable
		description: 'Immutable objects not supported by VM'