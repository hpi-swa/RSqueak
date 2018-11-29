overrides
buildCodePaneWith: builder
	
	| textSpec top controlButtons browseButtons annoSpec |
	top := builder pluggablePanelSpec new.
	top children: OrderedCollection new.
	
	controlButtons := self buildControlButtonsWith: builder.
	controlButtons frame: self controlButtonsFrame.
	top children add: controlButtons.
	
	self wantsOptionalButtons ifTrue: [
		browseButtons := self buildOptionalButtonsWith: builder.
		browseButtons frame: self optionalButtonsFrame.
		top children add: browseButtons].

	textSpec := builder pluggableCodePaneSpec new.
	textSpec 
		model: self;
		getText: #contents; 
		setText: #contents:notifying:; 
		selection: #contentsSelection; 
		menu: #codePaneMenu:shifted:;
		frame: self textFrame;
		stylerClass: FLTextStyler.
	top children add: textSpec.

	self wantsAnnotationPane ifTrue: [
		annoSpec := self buildAnnotationPaneWith: builder.
		annoSpec frame: self annotationFrame.
		top children add: annoSpec].
	.
	^ top