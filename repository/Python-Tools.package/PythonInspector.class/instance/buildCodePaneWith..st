overrides
buildCodePaneWith: builder
	| textSpec |
	textSpec := PythonPluggableCodePaneSpec new.
	textSpec 
		model: self;
		getText: #expression; 
		editText: #expression:;
		help: #helpText;
		selection: #contentsSelection; 
		menu: #codePaneMenu:shifted:;
		askBeforeDiscardingEdits: false.
	^textSpec