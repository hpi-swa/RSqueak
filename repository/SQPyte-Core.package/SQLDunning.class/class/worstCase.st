as yet unclassified
worstCase
	| db result cursor1 item kunnr |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := SQLDatabase open: (self dataPath, 'energy.db').
	
	result := OrderedCollection new.
	
	kunnr := OrderedCollection new.
	cursor1 := db execute: self worstCaseListSql.
	[ item := cursor1 next ] whileNotNil: [ kunnr add: item ].

	kunnr do: [ :knr | | customerID beleg cursor2 |
		beleg := OrderedCollection new.
		customerID := knr first.
		cursor2 := db execute: (self worstCaseInfoSql: customerID).
		[ item := cursor2 next ] whileNotNil: [ beleg add: item ].
		beleg do: [ :bel |
			| overdue cursor3 hasDebit cursor4 |
			FileStream stdout nextPut: $..
			cursor3 := db execute: (self worstCaseOverdueSql: bel first).
			overdue := cursor3 next first = 1.
			cursor4 := db execute: (self worstCaseCreditDebitSql: customerID).
			hasDebit := cursor4 next first = 1.
			
			(overdue and: [ hasDebit ])
				ifTrue: [ result add: {customerID. bel} ] ] ].

	FileStream stdout nextPutAll: result asString.
	^ result
