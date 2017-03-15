overrides
buildCodePaneWith: builder
	| textSpec top buttonSpec annoSpec |
	self wantsOptionalButtons ifTrue: [
		top := builder pluggablePanelSpec new.
		top children: OrderedCollection new.
		buttonSpec := self buildOptionalButtonsWith: builder.
		buttonSpec frame: self optionalButtonsFrame.
		top children add: buttonSpec].
	textSpec := builder pluggableCodePaneSpec new.
	textSpec 
		model: self;
		getText: #contents; 
		setText: #contents:notifying:; 
		selection: #contentsSelection; 
		menu: #codePaneMenu:shifted:;
		stylerClass: ForeignLanguageTextStyler.
	self wantsAnnotationPane ifTrue: [
		top ifNil: [
			top := builder pluggablePanelSpec new.
			top children: OrderedCollection new].
		annoSpec := self buildAnnotationPaneWith: builder.
		annoSpec frame: self annotationFrame.
		top children add: annoSpec].
	top ifNotNil: [
		textSpec frame: self textFrame.
		top children add: textSpec].
	^top ifNil: [textSpec]