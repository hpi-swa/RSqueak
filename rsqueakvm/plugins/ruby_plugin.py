from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.storage_classes import ClassShadow
from rsqueakvm.storage import AbstractCachingShadow
from rsqueakvm.primitives.bytecodes import EXTERNAL_CALL

from topaz.objspace import ObjectSpace
from topaz.objects.floatobject import W_FloatObject
from topaz.objects.intobject import W_FixnumObject
from topaz.objects.stringobject import W_StringObject
from topaz.objects.symbolobject import W_SymbolObject
from topaz.objects.nilobject import W_NilObject
from topaz.objects.boolobject import W_TrueObject, W_FalseObject
from topaz.objects.exceptionobject import W_SystemExit
from topaz.error import RubyError, print_traceback

from rpython.rlib import objectmodel, jit


RubyPlugin = Plugin()
ruby_space = ObjectSpace(None)


def startup(space, argv):
    try:
        ruby_space.setup(argv[0])
    except RubyError as e:
        print_traceback(ruby_space, e.w_value)
PluginStartupScripts.append(startup)


@objectmodel.specialize.argtype(0)
def wrap(interp, wr_object):
    space = interp.space
    if isinstance(wr_object, W_FloatObject):
        return space.wrap_float(ruby_space.float_w(wr_object))
    elif isinstance(wr_object, W_FixnumObject):
        return space.wrap_smallint_unsafe(ruby_space.int_w(wr_object))
    elif isinstance(wr_object, W_StringObject):
        return space.wrap_string(ruby_space.str_w(wr_object))
    elif isinstance(wr_object, W_NilObject):
        return space.w_nil
    elif isinstance(wr_object, W_FalseObject):
        return space.w_false
    elif isinstance(wr_object, W_TrueObject):
        return space.w_true
    elif isinstance(wr_object, W_SymbolObject):
        w_string = space.wrap_string(ruby_space.str_w(wr_object))
        return interp.perform(w_string, selector="asSymbol")
    else:
        return W_RubyObject(wr_object)

@objectmodel.specialize.argtype(0)
def unwrap(interp, w_object):
    space = interp.space
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
            w_Symbol = space.special_object("w_doesNotUnderstand").getclass(space)
            if w_object.getclass(space).is_same_object(w_Symbol):
                return ruby_space.newsymbol(space.unwrap_string(w_object))
    raise PrimitiveFailedError

class W_RubyObject(W_AbstractObjectWithIdentityHash):
    _attrs_ = ["wr_object"]
    _immutable_fields_ = ["wr_object"]
    repr_classname = "W_RubyObject"

    def __init__(self, wr_object):
        self.wr_object = wr_object

    def getclass(self, space):
        return W_RubyObject(self.wr_object.getclass(ruby_space))

    def class_shadow(self, space):
        return RubyClassShadow(space, ruby_space.getclass(self.wr_object))

    def is_same_object(self, other):
        return isinstance(other, W_RubyObject) and (other.wr_object is self.wr_object)

class RubyClassShadow(ClassShadow):
    _attrs_ = ["wr_class"]
    _immutable_fields_ = ["wr_class"]
    def __init__(self, space, wr_class):
        self.wr_class = wr_class
        AbstractCachingShadow.__init__(self, space, space.w_nil, 0, space.w_nil)

    def lookup(self, w_selector):
        return self._lookup(w_selector, self.wr_class.version)

    @jit.elidable
    def _lookup(self, w_selector, version):
        methodname = self.space.unwrap_string(w_selector)
        idx = methodname.find(":")
        if idx > 0:
            methodname = methodname[0:idx]
        ruby_method = self.wr_class.find_method(ruby_space, methodname)
        if ruby_method is None:
            return None
        if self.space.is_spur.is_set():
            w_cm = W_SpurCompiledMethod(self.space, bytecount=0, header=0)
        else:
            w_cm = W_PreSpurCompiledMethod(self.space, bytecount=0, header=0)
        w_cm._primitive = EXTERNAL_CALL
        w_cm.literalsize = 2
        w_cm.argsize = ruby_method.arity(ruby_space)
        w_cm.literals = [
            self.space.wrap_list([
                self.space.wrap_string("RubyPlugin"),
                self.space.wrap_string("send")
            ]),
            self.space.wrap_string(methodname)
        ]
        return w_cm


@RubyPlugin.expose_primitive(unwrap_spec=[object, str])
def eval(interp, s_frame, w_rcvr, source):
    try:
        return wrap(interp, ruby_space.execute(source))
    except RubyError as e:
        print_traceback(ruby_space, e.w_value)
        raise PrimitiveFailedError

@RubyPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    args_w = s_frame.peek_n(argcount)
    w_literal2 = w_method.literalat0(interp.space, 2)
    methodname = ""
    if isinstance(w_literal2, W_BytesObject):
        wr_rcvr = unwrap(interp, s_frame.peek(argcount))
        methodname = interp.space.unwrap_string(w_literal2)
    elif argcount == 3:
        methodname = interp.space.unwrap_string(args_w[0])
        wr_rcvr = unwrap(interp, args_w[1])
        args_w = interp.space.unwrap_array(args_w[2])
    else:
        raise PrimitiveFailedError
    args_rw = [unwrap(interp, w_arg) for w_arg in args_w]
    try:
        return wrap(interp, ruby_space.send(wr_rcvr, methodname, args_w=args_rw))
    except RubyError as e:
        print_traceback(ruby_space, e.w_value)
        raise PrimitiveFailedError
