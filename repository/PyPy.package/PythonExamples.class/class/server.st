debugging
server
	"Use user interrupt to interrupt server loop"
	Python exec: 'from helloworld import live_value, app'.
	^ Python eval: 'app.run()'