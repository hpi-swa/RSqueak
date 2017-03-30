helpers
hasAttribute: anAttribute
	^ (Python builtins hasattr __call__: self attr: anAttribute) asSmalltalk