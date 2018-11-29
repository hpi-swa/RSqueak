instance creation
addToolColors: theme
	"Tool-specific colors."
	
	"SUnit's TestRunner."
	theme 
		set: #failureColor for: #TestRunner to: self yellow;
		set: #errorColor for: #TestRunner to: self red;
		set: #passColor for: #TestRunner to: self green;
		
		derive: #failureTextColor for: #TestRunner from: #PluggableTextMorph at: #textColor;
		derive: #errorTextColor for: #TestRunner from: #PluggableTextMorph at: #textColor;
		derive: #passTextColor for: #TestRunner from: #PluggableTextMorph at: #textColor.
		
	"Monticello Tools."
	theme
		set: #revertedOperationAttributes for: #MCOperationsBrowser to: {TextEmphasis struckOut};
		set: #ignoredOperationAttributes for: #MCOperationsBrowser to: {TextColor color: Color gray}.
		"set: #rejectedOperationAttributes for: #MCOperationsBrowser to: {TextEmphasis struckOut};
		set: #acceptedOperationAttributes for: #MCOperationsBrowser to: {TextEmphasis underlined};
		set: #conflictingOperationAttributes for: #MCOperationsBrowser to: {TextEmphasis bold}."