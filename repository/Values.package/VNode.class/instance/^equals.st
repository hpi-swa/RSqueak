comparing
= other

	^ other class == self class  
	and: [other left = self left
	and: [other val = self val
	and: [other right = self right]]]