instance creation
create
	"self create apply."
	
	^ (self named: 'Python') in: [:theme |		
		"General morph stuff."
		theme
			set: #keyboardFocusColor for: #Morph to: self highlightColor;
			set: #keyboardFocusWidth for: #Morph to: 1;
			set: #softShadowColor for: #Morph to: (Color black alpha: 0.01);
			set: #softShadowOffset for: #Morph to: (10@8 corner: 10@12);
			set: #hardShadowColor for: #Morph to: (Color black alpha: 0.5);
			set: #hardShadowOffset for: #Morph to: 1@1.
			
		theme set: #background for: #MorphicProject to: self backgroundForm.
			
		self
			addFonts: theme;
			addWindowColors: theme;
			addSyntaxHighlighting: theme;
			addMenusAndDockingBars: theme;
			addDialogs: theme;
			addButtons: theme;
			addScrollables: theme;
			addToolColors: theme.
		
		theme]