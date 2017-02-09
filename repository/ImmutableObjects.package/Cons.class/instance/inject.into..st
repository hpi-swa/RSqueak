collection api
inject: anObject into: aBlock

	^ self foldl: aBlock withInit: anObject