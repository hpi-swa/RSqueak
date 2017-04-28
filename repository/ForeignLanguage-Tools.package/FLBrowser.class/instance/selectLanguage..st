foreign-language-support
selectLanguage: aLanguageSymbol
	self languageSymbol: aLanguageSymbol.
	self changed: #systemCategoryList.
	self changed: #relabel