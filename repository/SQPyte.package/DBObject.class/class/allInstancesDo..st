as yet unclassified
allInstancesDo: aBlock
	| handle obj |
	[ handle := self primitiveSQLAllInstances ]
		on: Error do: [ ^ super allInstancesDo: aBlock ].
	[ obj := self primitiveSQLNextObject: handle ]
		whileNotNil: [ aBlock value: obj ]