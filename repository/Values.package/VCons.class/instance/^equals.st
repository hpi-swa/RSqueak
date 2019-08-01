comparing
= other

	^ other class = self class
	and: [other car = self car
	and: [other cdr = self cdr]]