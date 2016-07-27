accessing
runWithNrClusters: aNumber
	| clusters centers changed minDist bestCluster dist sum |
	
	self nrClusters: aNumber.
	
	clusters := Array new: self nrClusters.
	centers := Array new: self nrClusters.
	
	1 to: self nrClusters do: [ :i | 
		clusters at: i put: (OrderedCollection new).
		centers at: i put: (self data at: i) ].
	
	changed := true.
	[changed] whileTrue: [
		"assign all values to clusters"
		1 to: self nrClusters do: [ :i | 
			clusters at: i put: (OrderedCollection new).
		].
		self data do: [ :datum |
			minDist := 1000000.
			bestCluster := 0.
			1 to: self nrClusters do: [ :cluster |
				dist := (centers at: cluster) dist: datum.
				dist < minDist ifTrue: [
					minDist := dist.
					bestCluster := cluster.
				].
			].
			(clusters at: bestCluster) add: datum.
			datum cluster: bestCluster.
		].
		"Re-calculate cluster means"
		changed := false.
		1 to: self nrClusters do: [ :cluster | 
			sum := 0@0.
			(clusters at: cluster) do: [ :point |
				sum := sum + point toPoint.
			].
			sum := sum / ((clusters at: cluster) size).
			(((centers at: cluster) dist: sum) < 1) ifFalse: [
				changed := true.
				centers at: cluster put: sum.
			].
		].
	].

	^ clusters.

	
	
	
	
	
	
	
	