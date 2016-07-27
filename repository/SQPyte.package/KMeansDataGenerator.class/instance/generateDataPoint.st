as yet unclassified
generateDataPoint
	| point |
	point := KMeansDataPoint new.
	point x: (self random nextInt: self maxValue).
	point y: (self random nextInt: self maxValue).
	^ point.