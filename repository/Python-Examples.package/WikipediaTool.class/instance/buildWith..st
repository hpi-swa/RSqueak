as yet unclassified
buildWith: builder
	^ builder build: (builder pluggableWindowSpec new
		model: self;
		children: {
			self buildSummary: builder.
			self buildSearchPanel: builder.
		};
		label: #label)