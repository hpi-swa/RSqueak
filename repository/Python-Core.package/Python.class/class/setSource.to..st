source code
setSource: pyFrame to: aString
	| filename oldContents lineno newContents stream |
	filename := pyFrame f_code co_filename asSmalltalk.
	stream := StandardFileStream readOnlyFileNamed: filename.
	oldContents := stream contents.
	stream close.
	lineno := pyFrame f_code co_firstlineno asSmalltalk.
	newContents := self replaceInPySource: oldContents content: aString lineno: lineno.
	stream := StandardFileStream forceNewFileNamed: filename.
  	stream := MultiByteFileStream newFrom: stream.
	stream write: newContents.
	stream close