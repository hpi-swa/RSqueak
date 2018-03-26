helpers
isClass
	^ (Python builtins isinstance __call__: self and: Python type) asSmalltalk