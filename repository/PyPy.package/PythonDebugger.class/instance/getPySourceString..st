overrides
getPySourceString: aContext
	"Attept to lookup source in sender method"
	| sender |
	sender := aContext sender.
	(sender receiver = Python and: [ sender selector = #fakeResumeFrame])
		ifFalse: [ ^ 'Nothing to show' ].
	sender := sender sender.
	((sender method literalAt: 1) = #primEvalInThread:cmd:)
		ifTrue: [ ^ sender method literalAt: 3 "Eval py source" ].
	^ 'Unable to find Python source'