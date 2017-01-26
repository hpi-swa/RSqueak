pages
ruby
	"This method was automatically generated. Edit it using:"
	"RSqueakHelp edit: #ruby"
	^(HelpTopic
		title: 'Ruby in RSqueak'
		contents: 
'RSqueak is written in RPython, and since this language is very close to Python, we can use external RPython code just like we can use Python libraries on Python. Since mose RPython code is in other virtual machines (such as PyPy (a Python VM), Pycket (a Racket VM), SQPyte (a SQlite VM), HippyVM (a PHP VM), or Topaz (a Ruby VM)) it is trivial for RSqueak/VM to import these VMs as "sub-systems" inside our Squeak VM execution engine. The only work is to add a nice Smalltalk API to interact with these other systems.

We have done this work for SQPyte and Topaz, and this release includes the Topaz Smalltalk API.

The API is very simple. There is a Ruby class which shows the two primitives that you can use from Smalltalk to run Ruby code. The other part is the RubyObject. This class is the runtime class for Squeak for all non-trivial Ruby objects.

You do not create instances of RubyObject directly, instead, any Ruby code result is either converted into an equivalent class (Float <-> Float, Fixnum <-> SmallInteger, String <-> String, nil <-> nil, true <-> true, false <-> false, Symbol <-> Symbol) or wrapped in a RubyObject proxy.

Any message sent to an instance of RubyObject is first looked up on the Ruby side of the underlying object. Only is none is found is lookup redirected to the Smalltalk code.

One way to keep Ruby code in the image is to wrap the Ruby class in a Compiler subclass that uses primEval: to create Ruby methods.
!
]style[(224 4 16 7 15 7 15 8 15 6 334 10 104 10 597 4 40 9 25),Rhttp://pypy.org/;,,Rhttps://github.com/pycket/pycket;,,Rhttps://arxiv.org/abs/1512.03207;,,Rhttp://hippyvm.baroquesoftware.com/;,,Rhttp://docs.topazruby.com/en/latest/;,,Rcode://Ruby class browse;b,,Rcode://RubyObject browse;b,,b,,i,!' readStream nextChunkText)
			key: #ruby