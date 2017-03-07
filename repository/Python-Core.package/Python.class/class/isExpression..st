helpers
isExpression: aPySource
	^ (self eval: '_isExpression("""',
aPySource, '""")') asSmalltalk