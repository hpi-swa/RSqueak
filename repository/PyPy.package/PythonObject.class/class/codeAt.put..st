as yet unclassified
codeAt: aSymbol put: pyCode
	"Code := nil"
	Code ifNil: [ Code := Dictionary new ].
	^ Code at: aSymbol put: pyCode