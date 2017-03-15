explorer
explorerContents
	^ self allInstVarNames asOrderedCollection collect: [ :attrName | | value |
		value := Python builtins getattr: self attrName: attrName.
		ObjectExplorerWrapper
			with: value
			name: attrName
			model: self ]