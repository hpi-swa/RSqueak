copying
copyWithoutIndex: index

	^ (self first: index -1), (self allButFirst: index)