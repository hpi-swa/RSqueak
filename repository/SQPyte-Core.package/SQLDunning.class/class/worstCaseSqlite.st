as yet unclassified
worstCaseSqlite
  | db result kunnr |
  FileStream startUp: true.
  FileStream stdout nextPutAll: 'Start'; cr.
  db := (SQLiteConnection fileNamed: (self dataPath, 'energy.db')) open.
  result := OrderedCollection new.
  kunnr := db executeQuery: self worstCaseListSql.
  FileStream stdout nextPutAll: 'foo'.
  kunnr do: [:kunde |
    | customerId belege |
      customerId := kunde at: 'KUNNR'.
      belege := db executeQuery: (self worstCaseInfoSql: customerId).
      belege do: [:beleg |
          | overdue cursor3 hasDebit cursor4 |
          FileStream stdout nextPut: $..
          cursor3 := db executeQuery: (self worstCaseOverdueSql: (beleg at: 'BELNR')).
          overdue := cursor3 isEmpty ifFalse: [cursor3 first anyOne = '1'].
          cursor4 := db executeQuery: (self worstCaseCreditDebitSql: customerId).
          hasDebit := cursor4 first anyOne = '1'.

          (overdue and: [hasDebit])
            ifTrue: [result add: {customerId. beleg}]]].
  FileStream stdout nextPutAll: result asString; cr.
  ^ result
