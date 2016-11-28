*ImmutableObjects
allImmutableInstances

	^ self allInstances select: [:ea | ea isImmutable]