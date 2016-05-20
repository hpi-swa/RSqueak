as yet unclassified
worstCaseSqlite
	| t1 t2 t3 |
	FileStream startUp: true.
	FileStream stdout nextPutAll: 'Start';
		 cr.
	t1 := (SQLiteConnection fileNamed: (self dataPath, 'energy.db')) open.
	t2 := OrderedCollection new.
	t3 := t1 executeQuery: self worstCaseListSql.
	t3
		do: [:t4 | 
			| t5 t6 |
			t5 := t4 at: 'KUNNR'.
			t6 := t1
						executeQuery: (self worstCaseInfoSql: t5).
			t6
				do: [:t7 | 
					| t8 t9 t10 t11 |
					FileStream stdout nextPut: $..
					t9 := t1
								executeQuery: (self worstCaseOverdueSql: t7 first).
					t8 := t9 isEmpty
								ifFalse: [1 halt.
									t9 first first = 1].
					t11 := t1
								executeQuery: (self worstCaseCreditDebitSql: t5).
					t10 := t11 at: 'SUM(DEBIT) > SUM(CREDIT)' = 1.
					(t8
							and: [t10])
						ifTrue: [t2 add: {t5. t7}]]].
	^ t2