helpers
vmSpeaksLanguage
	^ VMSpeaksPython ifNil: [ VMSpeaksPython := self basicVmSpeaksPython ]