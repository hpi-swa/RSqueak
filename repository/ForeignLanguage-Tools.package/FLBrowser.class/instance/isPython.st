helpers
isPython
	| class selector |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	class isPython ifFalse: [^ false].
	selector ifNil: [ ^ true ].
	^ class pyMethodDict includesKey: selector