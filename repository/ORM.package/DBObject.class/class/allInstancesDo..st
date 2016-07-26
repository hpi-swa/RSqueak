as yet unclassified
allInstancesDo: aBlock
	| handle obj |
	handle := self primitiveSQLAllInstances.
	[ obj := self primitiveSQLNextObject: handle ]
		whileNotNil: [ aBlock value: obj ]