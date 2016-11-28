testing
testImmutableObjectVMSupport

	self
		assert: (Object new immutableCopy) isImmutable
		description: 'Immutable objects not supported by VM'