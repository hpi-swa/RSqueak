helpers
hasPyCodeSelected
	| class selector |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	class isPython ifFalse: [^ false].
	^ class pyMethodDict includesKey: selector