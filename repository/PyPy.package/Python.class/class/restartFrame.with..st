experimental
restartFrame: pyFrame with: aSource
	self
		primRestartSpecificFrame: pyFrame
		source: aSource
		filename: pyFrame f_code co_filename
		cmd: 'exec'