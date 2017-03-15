overrides
selectedMessage
	"Answer a copy of the source code for the selected message."

	| class selector |
	contents == nil ifFalse: [^ contents copy].

	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	
	^ class sourceCodeAt: selector ifAbsent: [super selectedMessage].