as yet unclassified
worstCaseIn: aDatabaseFile usingSQPyte: aBool
	| db results time cursor1 item kunnr |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := SQLDatabase open: './', aDatabaseFile usingSQPyte: aBool.
	results := OrderedCollection new.
	
	time := [
	kunnr := OrderedCollection new.
	cursor1 := db execute: self worstCaseListSql.
	[ item := cursor1 next ] whileNotNil: [ kunnr add: item ].

	kunnr do: [ :knr | | customerID beleg cursor2 |
		beleg := OrderedCollection new.
		customerID := knr first.
		cursor2 := db execute: (self worstCaseInfoSql: customerID).
		[ item := cursor2 next ] whileNotNil: [ beleg add: item].
		beleg do: [ :bel |
			| overdue cursor3 hasDebit cursor4 |
			FileStream stdout nextPut: $..
			cursor3 := db execute: (self worstCaseOverdueSql: bel first).
			overdue := cursor3 next first = 1.
			cursor4 := db execute: (self worstCaseCreditDebitSql: customerID).
			hasDebit := cursor4 next first = 1.
			
			(overdue and: [ hasDebit ])
				ifTrue: [ results add: {customerID. bel} ] ] ] ] timeToRun.

	FileStream stdout cr; nextPutAll: results size asString, ' total results in ', time asString, 'ms'; cr.
	^ results
