private
doesNotUnderstand: aMessage
	self selectorList add: aMessage selector.
	self argumentList add: aMessage arguments