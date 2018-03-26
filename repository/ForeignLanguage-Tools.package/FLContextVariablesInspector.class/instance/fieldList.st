overrides
fieldList
	self object isForeign ifFalse: [ ^ super fieldList ].
	^fieldList ifNil:[fieldList := #('- thisContext -' '- filename -'), object tempNames]