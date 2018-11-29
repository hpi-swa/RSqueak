overrides
allInstances
	^ super allInstances"
	^ Python eval: self name, '.instances.values()'"