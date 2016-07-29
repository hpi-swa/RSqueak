sql-tpch
testQuery14
	^ 'select ',
			'100.00 * sum(case ',
				'when p.type like ''PROMO%'' ',
					'then l.extendedprice * (1 - l.discount) ',
				'else 0 ',
			'end) / sum(l.extendedprice * (1 - l.discount)) as promo_revenue ',
		'from ',
			'lineitem l, ',
			'part p ',
		'where ',
			'l.partkey = p.partkey ',
			'and l.shipdate >= date(''1995-01-01'') ',
			'and l.shipdate < date(''1995-01-01'', ''+1 month'');'