as yet unclassified
worstCaseSQLitePluginPreparedIn: aDatabaseFile
	| db results time kunnr |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start'; cr.
	db := (SQLiteConnection fileNamed: (FileStream fullName: aDatabaseFile)) open.
	results := OrderedCollection new.
	
	time := [
	kunnr := db executeQuery: self worstCaseListSql.

	kunnr do: [ :knr | | customerID beleg |
		beleg := OrderedCollection new.
		customerID := knr at: 'KUNNR'.
		beleg := db execute: self worstCaseInfoSql args: {customerID}.
		beleg do: [ :bel |
			| overdue hasDebit |
			FileStream stdout nextPut: $..
			overdue := (db execute: self worstCaseOverdueSql args: {bel at: 'BELNR'}) first = 1.
			hasDebit := (db execute: self worstCaseCreditDebitSql args: {customerID}) first = 1.
			
			(overdue and: [ hasDebit ])
				ifTrue: [ results add: {customerID. bel} ] ] ] ] timeToRun.

	FileStream stdout cr; nextPutAll: results size asString, ' total results in ', time asString, 'ms'; cr.
	^ results
