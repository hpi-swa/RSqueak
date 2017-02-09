collection api
reject: aBlock

	^ self filter: [:each | (aBlock value: each) not]