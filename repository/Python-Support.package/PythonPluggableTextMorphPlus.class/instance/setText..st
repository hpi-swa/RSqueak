as yet unclassified
setText: aText

	self okToStyle ifFalse:[^super setText: aText].
	super setText: (styler format: aText asText).
	self stylePyCode
		ifTrue: [ styler stylePython: textMorph contents ]
		ifFalse: [ styler style: textMorph contents ]