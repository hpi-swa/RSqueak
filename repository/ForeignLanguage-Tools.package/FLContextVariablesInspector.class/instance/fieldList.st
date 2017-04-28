overrides
fieldList
	self object isForeign ifFalse: [ ^ super fieldList ].
	^fieldList ifNil:[fieldList := (Array with: 'thisContext' with: 'foreign frame' with: 'filename') , object tempNames]