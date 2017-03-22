debugging
restartFrame: pyFrame with: aSource
	self
		primRestartSpecificFrame: pyFrame
		source: aSource
		filename: pyFrame f_code co_filename
		cmd: (self cmdFor: aSource)