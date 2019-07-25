accessing
last

	| prev |
	prev := VNil nil.
	^ self findf: [:ea | prev := ea. false "continue to end"] ifNone: [prev]