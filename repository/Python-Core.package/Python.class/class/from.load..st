helpers
from: aPackageName load: aModuleName
	self from: aPackageName import: aModuleName.
	^ Python eval: aModuleName