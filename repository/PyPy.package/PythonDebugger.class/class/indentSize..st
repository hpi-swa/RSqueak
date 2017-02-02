pysource
indentSize: aLine
	^ ((aLine findFirst: [:ea | ea isSeparator not]) - 1) max: 0