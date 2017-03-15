debugging
interrupt: aProcess label: aString
	"Open a ForeignLanguage if none exists"
	ActiveWorld submorphs detect: [ :ea | ea isSystemWindow and: [ ea model class == ForeignLanguage ]]
		ifFound: [ super interrupt: aProcess label: aString ]
		ifNone: [ ForeignLanguage
					openInterrupt: aString
					onProcess: aProcess ]