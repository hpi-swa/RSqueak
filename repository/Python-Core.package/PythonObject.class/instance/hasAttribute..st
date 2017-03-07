helpers
hasAttribute: anAttribute
	^ (Python builtins hasattr: self attr: anAttribute) asSmalltalk