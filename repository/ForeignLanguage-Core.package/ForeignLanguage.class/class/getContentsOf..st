helpers
getContentsOf: aFileName
	| stream data |
	(FileStream isAFileNamed: aFileName) ifFalse: [ ^ 'File does not exist' ].
	stream := StandardFileStream oldFileNamed: aFileName.
  	stream := MultiByteFileStream newFrom: stream.
	data := stream contents.
	stream close.
	^ data