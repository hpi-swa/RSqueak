as yet unclassified
pythonize: t instVars: f clsVars: d
	| code instVars clsVars pySuperclass initSignature |
	instVars := Scanner new scanFieldNames: f.
	clsVars := Scanner new scanFieldNames: d.
	initSignature := 'self'.
	instVars do: [ :ea | initSignature := initSignature, ', ', ea, '=None'].

	pySuperclass := (self superclass = Object
		ifTrue: [ 'object' ]
		ifFalse: [ self superclass name asString ]).
	code := 'class ', t asString, '(', pySuperclass, '):', String cr.
	
	code := code, String tab, 'next_id = 0', String cr.
	code := code, String tab, 'instances = {}', String cr.
	
	"clsVars are currently class attributes"
	clsVars do: [ :ea | code := code, String tab, ea, ' = None', String cr].

	code := code, String tab, 'def __init__(', initSignature ,'):', String cr.
	code := code, String tab, String tab, 'self.__class__.next_id += 1', String cr.
	code := code, String tab, String tab, 'self.inst_id = self.__class__.next_id', String cr.
	code := code, String tab, String tab, 'self.__class__.instances[self.inst_id] = self', String cr.
	instVars do: [ :ea |
		code := code, String tab, String tab, 'self.', ea, ' = ', ea, String cr ].
	instVars ifEmpty: [ code := code, String tab, String tab, 'pass', String cr ].
	
	"instVars do: [ :ea |
		code := code, String tab, 'def ', ea, '(self, val): self.', ea, '_val = val' , String cr ]."
	
	Python exec: code