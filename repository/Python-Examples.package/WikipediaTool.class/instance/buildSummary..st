as yet unclassified
buildSummary: builder
	^ builder pluggableTextSpec new
		model: self;
		getText: #contents;
		readOnly: true;
		indicateUnacceptedChanges: false;
		menu: #codePaneMenu:shifted:;
		frame: (LayoutFrame new leftFraction: 0 offset: 0;
					 topFraction: 0 offset: 25;
					 rightFraction: 1 offset: 0;
					 bottomFraction: 1 offset: 0)