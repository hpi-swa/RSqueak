sql-tpch
testQuery17
	^ 'select ',
			'sum(l.extendedprice) / 7.0 as avg_yearly ',
		'from ',
			'lineitem l, ',
			'part p ',
		'where ',
			'p.partkey = l.partkey ',
			'and p.brand = ''Brand#45'' ',
			'and p.container = ''SM PKG'' ',
			'and l.quantity < ( ',
    				'select ',
        				'0.2 * avg(quantity) ',
    				'from ',
        				'lineitem ',
    				'where ',
        				'partkey = p.partkey ',
		');'