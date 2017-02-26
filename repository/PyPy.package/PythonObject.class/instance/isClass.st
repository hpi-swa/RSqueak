helpers
isClass
	^ (Python builtins isinstance: self and: Python type) asSmalltalk