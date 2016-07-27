*SQPyte-Core
testDBObject
	| obj1 obj2 |
	obj1 := TestDBObject new.
	obj1 var: 123.
		
	obj2 := TestDBObject new.
	obj2 secondVar: (Random new).
	^ obj2 secondVar next.