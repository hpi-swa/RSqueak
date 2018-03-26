as yet unclassified
buildSearchClearButton: builder
	^ builder pluggableActionButtonSpec new
		model: self;
		label: 'Search';
		action: #actionSearch;
		help: 'Clear your search.';
		frame: (0.9@0 corner: 1@1)