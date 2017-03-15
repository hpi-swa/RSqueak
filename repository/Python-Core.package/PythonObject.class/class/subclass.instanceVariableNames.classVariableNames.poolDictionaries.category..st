as yet unclassified
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
	Python vmSpeaksLanguage ifTrue: [ self pythonize: t instVars: f clsVars: d ].
	^ super subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat