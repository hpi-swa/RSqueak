overrides
buildCodePaneWith: builder
	| textSpec |
	textSpec := builder pluggableCodePaneSpec new.
	textSpec 
		model: self;
		getText: #expression; 
		editText: #expression:;
		help: #helpText;
		selection: #contentsSelection; 
		menu: #codePaneMenu:shifted:;
		askBeforeDiscardingEdits: false;
		stylerClass: FLTextStyler.
	^textSpec