overrides
nextID
	"NextID = nil"
	NextID ifNil: [ NextID := 0 ].
	NextID := NextID + 1.
	^ NextID