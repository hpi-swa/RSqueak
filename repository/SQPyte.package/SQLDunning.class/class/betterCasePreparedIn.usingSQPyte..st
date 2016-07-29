as yet unclassified
betterCasePreparedIn: aDatabaseFile usingSQPyte: aBool
	| db results time cursor1 kunnr |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := SQLDatabase open: './', aDatabaseFile usingSQPyte: aBool.
	results := OrderedCollection new.
	
	time := [
	cursor1 := db execute: self worstCaseListSql.
	[ kunnr := cursor1 next ] whileNotNil: [ | customerID beleg cursor2 |
		customerID := kunnr first.
		cursor2 := db execute: self worstCaseInfoSql args: {customerID}.
		[ beleg := cursor2 next ] whileNotNil: [
			| overdue cursor3 hasDebit cursor4 |
			FileStream stdout nextPut: $..
			cursor3 := db execute: self worstCaseOverdueSql args: {beleg first}.
			overdue := cursor3 next first = 1.
			cursor4 := db execute: self worstCaseCreditDebitSql args: {customerID}.
			hasDebit := cursor4 next first = 1.
			
			(overdue and: [ hasDebit ])
				ifTrue: [ results add: {customerID. beleg} ] ] ] ] timeToRun.

	FileStream stdout cr; nextPutAll: results size asString, ' total results in ', time asString, 'ms'; cr.
	^ results
