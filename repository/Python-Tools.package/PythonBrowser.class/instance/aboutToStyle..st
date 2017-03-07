overrides
aboutToStyle: aStyler
	"This is a notification that aStyler is about to re-style its text.
	Set the classOrMetaClass in aStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	| type |
	Python vmSpeaksPython ifFalse: [ ^ super aboutToStyle: aStyler ].
	self isModeStyleable ifFalse: [^false].
	type := self editSelection.
	(#(newClass editClass) includes: type) ifTrue: [^false].
	(#(newMessage editMessage) includes: type) ifFalse:[ ^false].
	^true