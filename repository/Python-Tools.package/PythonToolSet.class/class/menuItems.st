as yet unclassified
menuItems
	"Answer the menu items available for this tool set"
	^#(
		('Python browser' 			#openPythonClassBrowser)
		('Python workspace'		#openPythonWorkspace)
		('Squeak browser' 			#openClassBrowser)
		('Squeak workspace'		#openWorkspace)
		('file list'					#openFileList)
		('package pane browser' 	#openPackagePaneBrowser)
		('process browser' 			#openProcessBrowser)
		-
		('method finder'				#openSelectorBrowser)
		('message names'			#openMessageNames)
		-
		('simple change sorter'		#openChangeSorter)
		('dual change sorter'		#openDualChangeSorter)
	)
