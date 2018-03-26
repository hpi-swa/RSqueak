as yet unclassified
contents
	| command result |
	command := '/usr/bin/python ',
		(FileDirectory default / 'wiki.py') fullName,
		' "', self searchTerm asString, '"'.
	result := (PipeableOSProcess command: command)
			outputAndError.
	result second
		ifEmpty: [ ^ result first ]
		ifNotEmpty: [ :stderr | self error: stderr ]