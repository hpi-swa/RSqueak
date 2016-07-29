sql-tpch
testInnerJoin
	^ 'select
			part.name, supplier.name
		from
			partsupp, supplier, part
		where
			partsupp.suppkey = supplier.suppkey and
			part.partkey = partsupp.partkey;'