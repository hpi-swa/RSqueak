overrides
privateFormatPython: aText
	"Perform any formatting of aText necessary and answer either aText, or a formatted copy of aText"

	aText asString = Object sourceCodeTemplate
		ifTrue:[
			"the original source code template does not parse,
			replace it with one that does"
			^self parseablePySourceCodeTemplate asText].	
	^aText