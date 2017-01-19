helpers
pyIdentifier
	| instID |
	instID := self inst_id.
	^ self class __name__, '.instances[', instID, ']'.