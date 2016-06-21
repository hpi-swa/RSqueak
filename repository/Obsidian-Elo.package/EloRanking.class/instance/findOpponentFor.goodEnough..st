elo-ranking
findOpponentFor: aPlayer goodEnough: diff

	| closest closestPlayer |
	closest := 1000. "high enough. Elo scores max out near 200"

	self players do: [ :candidate | | currentDiff |
		(candidate ~= aPlayer) ifTrue: [
			currentDiff := (candidate score - aPlayer score) abs.
			(currentDiff < diff) ifTrue: [^candidate].
			(currentDiff < closest) ifTrue: [
				closest := currentDiff.
				closestPlayer := candidate]]].
	^ closestPlayer