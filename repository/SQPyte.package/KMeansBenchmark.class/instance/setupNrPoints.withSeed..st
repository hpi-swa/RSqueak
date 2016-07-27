accessing
setupNrPoints: nrPoints withSeed: aSeed
	| generator |

	generator := KMeansDataGenerator new.
	generator numDataPoints: nrPoints.
	generator seed: aSeed.
	data := generator generateData.