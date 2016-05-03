sql-tpch
testQuery06
	^ 'select ',
			'sum(l.extendedprice * l.discount) as revenue ',
		'from ',
			'lineitem l ',
		'where ',
			'l.shipdate >= date(''1996-01-01'') ',
			'and l.shipdate < date(''1996-01-01'', ''+1 year'') ',
			'and l.discount between 0.04 and 0.07 ',
			'and l.quantity < 25;'