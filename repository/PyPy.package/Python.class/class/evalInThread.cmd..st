experimental
evalInThread: source cmd: evalOrExec
	| filename |
	filename := self persistPyCode: source.
	^ self primEvalInThread: source filename: filename cmd: evalOrExec