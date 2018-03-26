source code
persistEvalCode: aSource
	| directory filename stream |
	directory := 'eval', FileDirectory pathNameDelimiter.
	filename := directory, Time millisecondClockValue, self fileExtension.
	stream := StandardFileStream forceNewFileNamed: filename.
  	stream := MultiByteFileStream newFrom: stream.
	stream write: aSource.
	stream close.
	^ filename