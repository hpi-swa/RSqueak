overrides
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self pyString.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title