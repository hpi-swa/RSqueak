accessing
next
	| row object |
	row := self cursor next.
	row isNil ifTrue: [ ^ row ].
	
	object := self mapping new.
	
	row doWithIndex: [ :value :index | 
		object instVarNamed: (self columnNames at: index) put: value.
	].
	
	^ object