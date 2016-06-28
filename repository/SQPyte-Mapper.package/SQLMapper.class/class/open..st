instance creation
open: aSqliteConnectionString
	"Create new SQLMapper with a newly opened SQLDatabase"
	^ self using: (SQLDatabase open: aSqliteConnectionString usingSQPyte: true)