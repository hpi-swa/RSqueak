explorer
explorerContents
	^ self allInstVarNames asOrderedCollection collect: [ :attrName | | value |
		value := Python builtins getattr __call__: self attrName: attrName.
		ObjectExplorerWrapper
			with: value
			name: attrName
			model: self ]