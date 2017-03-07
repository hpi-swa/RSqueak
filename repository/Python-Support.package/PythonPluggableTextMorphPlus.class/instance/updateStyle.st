as yet unclassified
updateStyle
	self okToStyle ifTrue: [
		self stylePyCode
			ifTrue: [ styler stylePythonInBackgroundProcess: textMorph contents ]
			ifFalse: [ styler styleInBackgroundProcess: textMorph contents ] ]