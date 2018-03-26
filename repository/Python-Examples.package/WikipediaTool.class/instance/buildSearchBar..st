as yet unclassified
buildSearchBar: builder
	^ builder pluggableInputFieldSpec new
		minimumHeight: 0;
		model: self;
		getText: #searchTerm;
		setText: #searchTerm:;
		indicateUnacceptedChanges: false;
		help: 'Search wikipedia...';
		frame: (0@0 corner: 0.9@1)