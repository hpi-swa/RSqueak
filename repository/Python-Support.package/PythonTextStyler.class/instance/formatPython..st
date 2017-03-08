overrides
formatPython: aText
	"Answer a copy of <aText> which has been reformatted,
	or <aText> if no formatting is to be applied"
	
	self terminateBackgroundStylingProcess.
	^self privateFormatPython: aText