overrides
systemCategoryList
	"Answer the class categories modelled by the receiver."

	^ super systemCategoryList select: [:ea | ea beginsWith: 'Python']