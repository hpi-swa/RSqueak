as yet unclassified
betterCase
	| db result cursor1 kunnr beleg |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := SQLDatabase open: (self dataPath, 'energy100.db').

	result := OrderedCollection new.
	
	cursor1 := db execute: self worstCaseListSql.
	[ kunnr := cursor1 next ] whileNotNil: [
		| customerID cursor2 |
		customerID := kunnr first.
		cursor2 := db execute: (self worstCaseInfoSql: customerID).
		[ beleg := cursor2 next ] whileNotNil: [
			| overdue cursor3 hasDebit cursor4 |
			FileStream stdout nextPut: $..
			cursor3 := db execute: (self worstCaseOverdueSql: beleg first).
			overdue := cursor3 next first = 1.
			cursor4 := db execute: (self worstCaseCreditDebitSql: customerID).
			hasDebit := cursor4 next first = 1.
			(overdue and: [ hasDebit ])
				ifTrue: [ result add: {customerID. beleg} ] ] ].

	^ result
