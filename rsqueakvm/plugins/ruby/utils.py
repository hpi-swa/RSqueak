from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.plugins.ruby.model import W_RubyObject
from rsqueakvm.plugins.ruby.objspace import ruby_space
from rsqueakvm.model.variable import W_BytesObject

from topaz.objects.floatobject import W_FloatObject as WR_FloatObject
from topaz.objects.intobject import W_FixnumObject as WR_FixnumObject
from topaz.objects.stringobject import W_StringObject as WR_StringObject
from topaz.objects.symbolobject import W_SymbolObject as WR_SymbolObject
from topaz.objects.nilobject import W_NilObject as WR_NilObject
from topaz.objects.boolobject import (
    W_TrueObject as WR_TrueObject, W_FalseObject as WR_FalseObject)

from rpython.rlib import objectmodel


def ruby_to_smalltalk(space, wr_object):
    if isinstance(wr_object, WR_FloatObject):
        return space.wrap_float(ruby_space.float_w(wr_object))
    elif isinstance(wr_object, WR_FixnumObject):
        return space.wrap_smallint_unsafe(ruby_space.int_w(wr_object))
    elif isinstance(wr_object, WR_StringObject):
        return space.wrap_string(ruby_space.str_w(wr_object))
    elif isinstance(wr_object, WR_NilObject):
        return space.w_nil
    elif isinstance(wr_object, WR_FalseObject):
        return space.w_false
    elif isinstance(wr_object, WR_TrueObject):
        return space.w_true
    elif isinstance(wr_object, WR_SymbolObject):
        return space.wrap_symbol(ruby_space.str_w(wr_object))
    print 'Cannot convert %s to Smalltalk' % wr_object
    raise PrimitiveFailedError


@objectmodel.specialize.argtype(0)
def smalltalk_to_ruby(space, w_object):
    if isinstance(w_object, W_RubyObject):
        return w_object.wr_object
    elif isinstance(w_object, W_Float):
        return ruby_space.newfloat(space.unwrap_float(w_object))
    elif isinstance(w_object, W_SmallInteger):
        return ruby_space.newint(space.unwrap_int(w_object))
    elif isinstance(w_object, W_BytesObject):
        if w_object.getclass(space).is_same_object(space.w_String):
            return ruby_space.newstr_fromstr(space.unwrap_string(w_object))
        else:
            w_Symbol = space.w_doesNotUnderstand.getclass(space)
            if w_object.getclass(space).is_same_object(w_Symbol):
                return ruby_space.newsymbol(space.unwrap_string(w_object))
    print 'Cannot convert %s to Ruby' % w_object
    raise PrimitiveFailedError
