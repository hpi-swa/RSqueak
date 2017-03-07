pysource
setPySourceContent: pyCode content: aString
	| filename oldContents newContents stream |
	filename := pyCode co_filename asSmalltalk.
	stream := StandardFileStream readOnlyFileNamed: filename.
	oldContents := stream contents.
	stream close.
	newContents := self replaceInPySource: oldContents content: aString lineno: pyCode co_firstlineno asSmalltalk.
	stream := StandardFileStream forceNewFileNamed: filename.
  	stream := MultiByteFileStream newFrom: stream.
	stream write: newContents.
	stream close
	