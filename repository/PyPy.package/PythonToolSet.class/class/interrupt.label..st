as yet unclassified
interrupt: aProcess label: aString
	"Open a PythonDebugger if none exists"
	ActiveWorld submorphs detect: [ :ea | ea isSystemWindow and: [ ea model class == PythonDebugger ]]
		ifFound: [ super interrupt: aProcess label: aString ]
		ifNone: [ PythonDebugger
					openInterrupt: aString
					onProcess: aProcess ]