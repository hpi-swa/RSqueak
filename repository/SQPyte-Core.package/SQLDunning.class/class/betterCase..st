as yet unclassified
betterCase: aDatabaseDriver
	| db results time cursor1 kunnr beleg |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := SQLDatabase open: './energy.db' with: aDatabaseDriver.
	results := OrderedCollection new.
	
	time := [
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
				ifTrue: [ results add: {customerID. beleg} ] ] ] ] timeToRun.

	FileStream stdout cr; nextPutAll: results size asString, ' total results in ', time asString, 'ms'; cr.
	^ results
