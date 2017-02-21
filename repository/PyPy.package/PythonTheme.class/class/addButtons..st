instance creation
addButtons: theme
	"self create apply"
	theme
		set: #borderColor for: #PluggableButtonMorph to: Color gray;
		set: #borderWidth for: #PluggableButtonMorph to: 0;
		set: #borderStyle for: #PluggableButtonMorph to: BorderStyle default;
		set: #color for: #PluggableButtonMorph to: self backgroundColor lighter;
		
		set: #font for: #PluggableButtonMorph to: [Preferences standardButtonFont];
		set: #textColor for: #PluggableButtonMorph to: self textColor;

		set: #selectionModifier for: #PluggableButtonMorph to: [ [:c | c adjustBrightness: 0.2] ];
		set: #hoverModifier for: #PluggableButtonMorph to: [ [:c | c adjustBrightness: 0.4] ];
		set: #feedbackModifier for: #PluggableButtonMorph to: [ [:c | c adjustBrightness: 0.6] ].
	
	"And the plus-version."
	theme
		set: #disabledColor for: #PluggableButtonMorphPlus to: Color transparent;
		set: #disabledTextColor for: #PluggableButtonMorphPlus to: (Color gray: 0.6).

	"And the three-phase button."
	theme
		derive: #color for: #ThreePhaseButtonMorph from: #PluggableButtonMorph at: #textColor;
		derive: #font for: #ThreePhaseButtonMorph from: #PluggableButtonMorph;
		derive: #textColor for: #ThreePhaseButtonMorph from: #PluggableButtonMorph.