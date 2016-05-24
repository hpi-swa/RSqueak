as yet unclassified
worstCaseSQLitePluginIn: aDatabaseFile
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
		beleg := db executeQuery: (self worstCaseInfoSql: customerID).
		beleg do: [ :bel |
			| overdue hasDebit |
			FileStream stdout nextPut: $..
			overdue := (db executeQuery: (self worstCaseOverdueSql: (bel at: 'BELNR'))) first anyOne = '1'.
			hasDebit := (db executeQuery: (self worstCaseCreditDebitSql: customerID)) first anyOne = '1'.
			
			(overdue and: [ hasDebit ])
				ifTrue: [ results add: {customerID. bel} ] ] ] ] timeToRun.

	FileStream stdout cr; nextPutAll: results size asString, ' total results in ', time asString, 'ms'; cr.
	^ results
