helpers
getContentsOf: aFileName
	| stream data |
	stream := StandardFileStream oldFileNamed: aFileName.
  	stream := MultiByteFileStream newFrom: stream.
	data := stream contents.
	stream close.
	^ data