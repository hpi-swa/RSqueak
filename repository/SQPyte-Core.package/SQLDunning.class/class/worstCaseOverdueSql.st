sql
worstCaseOverdueSql
	^ 'SELECT (julianday(''2014-12-20'') - julianday(BUDAT) > 14) FROM BKPF WHERE BELNR = ?;'