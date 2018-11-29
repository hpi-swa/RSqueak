accessing
kernel

	^ RubyKernel ifNil: [RubyKernel := self primEval: 'Kernel']