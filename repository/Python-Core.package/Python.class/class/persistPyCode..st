experimental
persistPyCode: pySource
	| filename stream |
	filename := 'py_files', FileDirectory pathNameDelimiter ,'source_', Time millisecondClockValue, '.py'.
	stream := StandardFileStream forceNewFileNamed: filename.
  	stream := MultiByteFileStream newFrom: stream.
	stream write: pySource.
	stream close.
	^ filename