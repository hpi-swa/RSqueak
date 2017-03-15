overrides
buildCodePaneWith: builder
	| textSpec |
	textSpec := builder pluggableCodePaneSpec new.
	textSpec 
		model: self;
		getText: #contents; 
		setText: #contents:notifying:; 
		selection: #contentsSelection; 
		menu: #codePaneMenu:shifted:;
		stylerClass: ForeignLanguageTextStyler.
	^textSpec