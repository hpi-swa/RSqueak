initialize-release
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
	| newClass |
	newClass := super subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat.
	self compilePrimitivesIn: newClass.
	^ newClass