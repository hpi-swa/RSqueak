overrides
addModelItemsToWindowMenu: aMenu
	aMenu addLine.
	ForeignLanguage availableLanguages do: [:ea |
		aMenu
			add: 'select ', ea asString ,'...'
			target: self
			selector: #selectLanguage:
			argument: ea ].
	aMenu addLine.

	super addModelItemsToWindowMenu: aMenu