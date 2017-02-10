instance creation
addWindowColors: theme
	"self create apply"
	theme 
		set: #titleFont for: #SystemWindow to: [Preferences windowTitleFont];
		set: #borderColorModifier for: #SystemWindow to: [ [:c | c adjustBrightness: -0.2] ];
		set: #borderWidth for: #SystemWindow to: 0;
	
		set: #uniformWindowColor for: #Model to: self windowColor;
		derive: #uniformWindowColor for: #TranscriptStream from: #Model;
		derive: #color for: #SystemWindow from: #Model at: #uniformWindowColor; "Fall back for windows without models."
				
		set: #unfocusedWindowColorModifier for: #SystemWindow to: [ [:color | color darker] ];
		set: #unfocusedLabelColor for: #SystemWindow to: self unfocusedLabelColor;
		set: #focusedLabelColor for: #SystemWindow to: self focusedLabelColor;

		set: #customWindowColor for: #Browser to: (Color r: 0.764 g: 0.9 b: 0.63);
		set: #customWindowColor for: #ChangeList to: (Color r: 0.719 g: 0.9 b: 0.9);
		set: #customWindowColor for: #ChangeSorter to: (Color r: 0.719 g: 0.9 b: 0.9);
		set: #customWindowColor for: #ChatNotes to: (Color r: 1.0 g: 0.7 b: 0.8);
		set: #customWindowColor for: #ClassCommentVersionsBrowser to: (Color r: 0.753 g: 0.677 b: 0.9);
		set: #customWindowColor for: #Debugger to: (Color r: 0.9 g: 0.719 b: 0.719);
		set: #customWindowColor for: #DualChangeSorter to: (Color r: 0.719 g: 0.9 b: 0.9);
		set: #customWindowColor for: #FileContentsBrowser to: (Color r: 0.7 g: 0.7 b: 0.508);
		set: #customWindowColor for: #FileList to: (Color r: 0.65 g: 0.65 b: 0.65);
		set: #customWindowColor for: #InstanceBrowser to: (Color r: 0.726 g: 0.9 b: 0.9);
		set: #customWindowColor for: #Lexicon to: (Color r: 0.79 g: 0.9 b: 0.79);
		set: #customWindowColor for: #MCTool to: (Color r: 0.65 g: 0.691 b: 0.876);
		set: #customWindowColor for: #MessageNames to: (Color r: 0.639 g: 0.9 b: 0.497);
		set: #customWindowColor for: #MessageSet to: (Color r: 0.719 g: 0.9 b: 0.9);
		set: #customWindowColor for: #PackagePaneBrowser to: (Color r: 0.9 g: 0.9 b: 0.63);
		set: #customWindowColor for: #PluggableFileList to: Color lightYellow;
		set: #customWindowColor for: #PreferenceBrowser to: (Color r: 0.671 g: 0.9 b: 0.9);
		set: #customWindowColor for: #SMLoader to: (Color r: 0.801 g: 0.801 b: 0.614);
		set: #customWindowColor for: #SMLoaderPlus to: (Color r: 0.801 g: 0.801 b: 0.614);
		set: #customWindowColor for: #SMReleaseBrowser to: (Color r: 0.801 g: 0.801 b: 0.614);
		set: #customWindowColor for: #ScriptingDomain to: (Color r: 0.91 g: 0.91 b: 0.91);
		set: #customWindowColor for: #SelectorBrowser to: (Color r: 0.45 g: 0.9 b: 0.9);
		set: #customWindowColor for: #StringHolder to: (Color r: 0.9 g: 0.9 b: 0.719);
		set: #customWindowColor for: #TestRunner to: (Color r: 0.9 g: 0.576 b: 0.09);
		set: #customWindowColor for: #TranscriptStream to: (Color r: 0.9 g: 0.75 b: 0.45);
		set: #customWindowColor for: #VersionsBrowser to: (Color r: 0.782 g: 0.677 b: 0.9).