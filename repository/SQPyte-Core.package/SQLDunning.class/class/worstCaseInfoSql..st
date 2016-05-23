as yet unclassified
worstCaseInfoSql: anKUNNR
	^ 'SELECT BELNR, BUZEI, GJAHR FROM BSEG JOIN BKPF USING (''MANDT'', ''BUKRS'', ''BELNR'', ''GJAHR'')
		WHERE AUGBL IS NULL
		AND KUNNR = ''', anKUNNR, ''';'