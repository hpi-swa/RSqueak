accessing
at: index

	^ self at: index ifAbsent: [self errorSubscriptBounds: index]