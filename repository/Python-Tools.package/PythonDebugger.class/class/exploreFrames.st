utilities
exploreFrames
	| currentFrame result frameInfo |
	currentFrame := Python primGetTopFrame.
	result := OrderedCollection new.
	[ currentFrame notNil ] whileTrue: [
		frameInfo := IdentityDictionary new
			add: #f_lineno->(currentFrame f_lineno);
			add: #co_name->(currentFrame f_code co_name);
			add: #co_firstlineno->(currentFrame f_code co_firstlineno);
			add: #co_filename->(currentFrame f_code co_filename);
			add: #f_locals->(currentFrame f_locals __str__);
			add: #f_globals->(currentFrame f_globals __str__);
			yourself.
		result add: frameInfo.
		currentFrame := currentFrame f_back ].
	result explore
	