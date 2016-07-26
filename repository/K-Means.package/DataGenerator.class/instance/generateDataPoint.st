as yet unclassified
generateDataPoint
	| point |
	point := DataPoint new.
	point x: (self random nextInt: self maxValue).
	point y: (self random nextInt: self maxValue).
	^ point.