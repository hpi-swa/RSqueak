helpers
getGlobalKeys
	^ Python eval: 'globals().keys()'
	