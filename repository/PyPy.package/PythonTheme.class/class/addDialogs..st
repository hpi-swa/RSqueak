instance creation
addDialogs: theme
	"self create apply."
	
	theme
		set: #borderColor for: #DialogWindow to: Color gray;
		set: #borderWidth for: #DialogWindow to: 1;
		set: #borderStyle for: #DialogWindow to: BorderStyle default;
		set: #color for: #DialogWindow to: self backgroundColor;

		set: #titleBorderColor for: #DialogWindow to: Color gray;
		set: #titleBorderWidth for: #DialogWindow to: 0;
		set: #titleBorderStyle for: #DialogWindow to: BorderStyle default;
		set: #titleColor for: #DialogWindow to: self highlightColor;
		set: #titleFont for: #DialogWindow to: [Preferences windowTitleFont];
		set: #titleTextColor for: #DialogWindow to: self textColor;
		
		set: #font for: #DialogWindow to: [Preferences standardSystemFont];
		set: #textColor for: #DialogWindow to: self textColor;
		
		set: #okColor for: #DialogWindow to: self green;
		set: #cancelColor for: #DialogWindow to: self red;
		set: #buttonColor for: #DialogWindow to: (self backgroundColor adjustBrightness: 0.2);
		set: #selectionModifier for: #DialogWindow to: [ [:c | c adjustBrightness: 0.1 ] ].
	
	"The List Chooser is a dialog, too."
	theme
		derive: #okColor for: #ListChooser from: #DialogWindow;
		derive: #cancelColor for: #ListChooser from: #DialogWindow;
		set: #addColor for: #ListChooser to: Color blue muchLighter;
		set: #disabledColor for: #ListChooser to: Color gray.

	"And the mulitple list chooser."
	theme
		derive: #okColor for: #ListMultipleChooser from: #DialogWindow;
		derive: #cancelColor for: #ListMultipleChooser from: #DialogWindow.
		
	"And the system progress bar."
	theme
		derive: #borderColor for: #SystemProgressMorph from: #MenuMorph;
		derive: #borderWidth for: #SystemProgressMorph from: #MenuMorph;
		derive: #borderStyle for: #SystemProgressMorph from: #MenuMorph;
		derive: #color for: #SystemProgressMorph from: #MenuMorph;
		derive: #font for: #SystemProgressMorph from: #MenuItemMorph;
		derive: #textColor for: #SystemProgressMorph from: #MenuItemMorph;
		
		set: #borderColor for: #SystemProgressBarMorph to: Color transparent;
		set: #borderWidth for: #SystemProgressBarMorph to: 0;
		set: #borderStyle for: #SystemProgressBarMorph to: BorderStyle default;
		set: #color for: #SystemProgressBarMorph to: (Color r: 0.977 g: 0.977 b: 0.977);
		set: #barColor for: #SystemProgressBarMorph to: (Color r: 0.72 g: 0.72 b: 0.9).
		
	"And the balloon morphs."
	theme
		set: #borderColor for: #NewBalloonMorph to: self highlightColor;
		set: #borderWidth for: #NewBalloonMorph to: 1;
		set: #color for: #NewBalloonMorph to: self highlightColor;
		set: #font for: #NewBalloonMorph to: [Preferences standardBalloonHelpFont];
		derive: #textColor for: #NewBalloonMorph from: #PluggableButtonMorph.
		
	theme
		derive: #borderColor for: #BalloonMorph from: #NewBalloonMorph;
		set: #borderWidth for: #BalloonMorph to: 0;
		derive: #color for: #BalloonMorph from: #NewBalloonMorph;
		derive: #font for: #BalloonMorph from: #NewBalloonMorph;
		derive: #textColor for: #BalloonMorph from: #NewBalloonMorph.