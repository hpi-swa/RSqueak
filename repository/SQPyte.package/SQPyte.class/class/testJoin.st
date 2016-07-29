sql-tpch
testJoin
	^ 'select S.name, N.name from Supplier S, Nation N where S.nationkey = N.nationkey;'