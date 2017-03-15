instance creation
addMenusAndDockingBars: theme
	"self create apply"
	theme
		set: #borderColor for: #MenuMorph to: Color gray;
		set: #borderWidth for: #MenuMorph to: 1;
		set: #borderStyle for: #MenuMorph to: BorderStyle default;
		set: #color for: #MenuMorph to: self backgroundColor;

		set: #titleBorderColor for: #MenuMorph to: self backgroundColor darker;
		set: #titleBorderWidth for: #MenuMorph to: 0;
		set: #titleBorderStyle for: #MenuMorph to: BorderStyle default;
		set: #titleColor for: #MenuMorph to: Color transparent;
		set: #titleFont for: #MenuMorph to: [Preferences windowTitleFont];
		set: #titleTextColor for: #MenuMorph to: self textColor;
		
		set: #lineColor for: #MenuMorph to: self backgroundColor darker;
		set: #lineStyle for: #MenuMorph to: BorderStyle simple;
		set: #lineWidth for: #MenuMorph to: 1.
		
	theme
		set: #font for: #MenuItemMorph to: [Preferences standardMenuFont];
		set: #textColor for: #MenuItemMorph to: self unfocusedLabelColor;
		set: #disabledTextColor for: #MenuItemMorph to: Color gray;
		set: #selectionColor for: #MenuItemMorph to: self backgroundColor lighter lighter;
		set: #selectionTextColor for: #MenuItemMorph to: self textColor.
		
	"Derive some stuff for the docking bar morph, which looks mostly like a menu morph."
	theme
		set: #borderWidth for: #DockingBarMorph to: 0;
		derive: #borderColor for: #DockingBarMorph from: #MenuMorph;
		derive: #borderStyle for: #DockingBarMorph from: #MenuMorph;
		derive: #color for: #DockingBarMorph from: #MenuMorph;
	
		derive: #lineColor for: #DockingBarMorph from: #MenuMorph;
		derive: #lineStyle for: #DockingBarMorph from: #MenuMorph;
		derive: #lineWidth for: #DockingBarMorph from: #MenuMorph.
		
	"The world main docking bar."
	theme
		derive: #font for: #TheWorldMainDockingBar from: #MenuItemMorph;
		derive: #textColor for: #TheWorldMainDockingBar from: #MenuItemMorph;
		set: #logoColor for: #TheWorldMainDockingBar to: self textColor;
		set: #selectionLogoColor for: #TheWorldMainDockingBar to: Color white.