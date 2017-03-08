pySource
indent: aString by: aNumber
	| indent |
	indent := String new: aNumber withAll: Character space.
	^ (aString lines collect: [:ea | indent, ea]) joinSeparatedBy: Character cr