import py
from spyvm import model, fieldtypes
from spyvm import objspace

from spyvm.fieldtypes import obj, SInt, LPI, flt

def test_simple_changes():
	a = fieldtypes.FieldTypes.of_length(3)
	assert a.types == [obj, obj, obj]
	b = a.sibling(1, SInt)
	assert b.types == [obj, SInt, obj]
	c = a.sibling(1, SInt)
	assert b is c

def test_two_level_changes_identity():
	a = fieldtypes.FieldTypes.of_length(3)
	b = a.sibling(1, SInt)
	c = a.sibling(0, SInt)
	d = b.sibling(0, SInt)
	assert d.types == [SInt, SInt, obj]
	e = c.sibling(1, SInt)
	assert d is e

def test_numberOfElements():
	a = fieldtypes.FieldTypes.of_length(3)
	a.sibling(0, SInt).sibling(1, SInt).sibling(2, SInt)
	a.sibling(1, SInt).sibling(2, SInt)
	a.sibling(2, SInt).sibling(0, SInt)
	assert a.sibling(2, SInt).sibling(0, SInt).parent is a.sibling(0, SInt)
	assert len(a.siblings) == 3
	assert len(a.sibling(0, SInt).siblings) == 2
	assert len(a.sibling(1, SInt).siblings) == 2
	assert len(a.sibling(2, SInt).siblings) == 1 # link to [o, i, i] not created

def test_multiple_changes():
	a = fieldtypes.FieldTypes.of_length(3)
	b = a.sibling(0, SInt)
	for tag in [LPI, flt]:
		assert b.sibling(0, tag).types == [tag, obj, obj]

def test_obj_replacement():
	a = fieldtypes.FieldTypes.of_length(3)
	b = a.sibling(0, SInt).sibling(1, SInt)
	c = a.sibling(1, SInt)
	assert b.sibling(0, obj) is c
