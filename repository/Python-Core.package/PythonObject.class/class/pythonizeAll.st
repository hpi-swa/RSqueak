as yet unclassified
pythonizeAll
	NextID := 0.
	"Pythonize all Python classes"
	self withAllSubclasses do: [ :ea | ea pythonize ].