accessing
dist: aDataPoint 
	| dx dy |

	dx := aDataPoint x - x.
	dy := aDataPoint y - y.

	^ ((dx * dx) + (dy * dy)) sqrt