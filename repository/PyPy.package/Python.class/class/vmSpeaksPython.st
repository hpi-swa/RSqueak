helpers
vmSpeaksPython
	^ VMSpeaksPython ifNil: [ VMSpeaksPython := self basicVmSpeaksPython ]