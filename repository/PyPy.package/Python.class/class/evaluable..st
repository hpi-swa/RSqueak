helpers
evaluable: aPySource
	(('*=*' match: aPySource) and: [ ('*==*' match: aPySource) not ]) ifTrue: [ ^ false ].
	(aPySource includesAnyOf: {$:. Character cr. Character lf}) ifTrue: [ ^ false ].
	^ true