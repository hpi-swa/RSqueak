accessing
last

	| prev |
	prev := nil.
	^ self findf: [:ea | prev := ea. false "continue to end"] ifNone: [prev]