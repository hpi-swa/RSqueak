accessing
object

	^ RubyObject ifNil: [RubyObject := self primEval: 'Object']