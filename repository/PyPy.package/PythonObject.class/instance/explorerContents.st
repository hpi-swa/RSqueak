explorer
explorerContents
	^ self allAttributes asOrderedCollection collect: [:each | | value |
		value := self perform: each.
		ObjectExplorerWrapper
			with: value
			name: each
			model: self]