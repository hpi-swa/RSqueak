import sys, weakref
from spyvm import model, constants, error, wrapper, version
from spyvm.version import elidable_for_version, constant_for_version, constant_for_version_arg
from rpython.tool.pairtype import extendabletype
from rpython.rlib import rarithmetic, objectmodel, jit, longlong2float
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.debug import make_sure_not_resized
from rpython.rlib.rstruct.runpack import runpack
from rpython.rtyper.lltypesystem import rffi, lltype

# If this is True, then no optimizing storage strategies will be used.
# Intended for performance comparisons. Breaks tests.
no_specialized_storage = False

class AbstractShadow(object):
    """A shadow is an optional extra bit of information that
    can be attached at run-time to any Smalltalk object.
    """
    _attrs_ = ['_w_self', 'space']
    _immutable_fields_ = ['space']
    provides_getname = False
    repr_classname = "AbstractShadow"
    
    def __init__(self, space, w_self):
        self.space = space
        assert w_self is None or isinstance(w_self, model.W_PointersObject)
        self._w_self = w_self
    def w_self(self):
        return self._w_self
    def getname(self):
        raise NotImplementedError("Abstract class")
    def __repr__(self):
        if self.provides_getname:
            return "<%s %s>" % (self.repr_classname, self.getname())
        else:
            return "<%s>" % self.repr_classname
    
    def fetch(self, n0):
        raise NotImplementedError("Abstract class")
    def store(self, n0, w_value):
        raise NotImplementedError("Abstract class")
    def size(self):
        raise NotImplementedError("Abstract class")
    
    def attach_shadow(self): pass
    
    def copy_field_from(self, n0, other_shadow):
        self.store(n0, other_shadow.fetch(n0))
    
    # This can be overwritten to change the order of initialization.
    def copy_from(self, other_shadow):
        assert self.size() == other_shadow.size()
        for i in range(self.size()):
            self.copy_field_from(i, other_shadow)

class AbstractStorageShadow(AbstractShadow):
    _attrs_ = []
    repr_classname = "AbstractStorageShadow"
    def __init__(self, space, w_self, size):
        AbstractShadow.__init__(self, space, w_self)
    def store(self, n0, w_val):
        if self.can_contain(w_val):
            return self.do_store(n0, w_val)
        new_storage = self.generalized_strategy_for(w_val)
        return self._w_self.store_with_new_storage(new_storage, n0, w_val)
    def can_contain(self, w_val):
        return self.static_can_contain(self.space, w_val)
    @staticmethod
    def static_can_contain(space, w_val):
        raise NotImplementedError()
    def do_store(self, n0, w_val):
        raise NotImplementedError()
    def generalized_strategy_for(self, w_val):
        raise NotImplementedError()

class AllNilStorageShadow(AbstractStorageShadow):
    repr_classname = "AllNilStorageShadow"
    _attrs_ = ['_size']
    _immutable_fields_ = ['_size']
    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self._size = size
    def fetch(self, n0):
        if n0 >= self._size:
            raise IndexError
        return self.space.w_nil
    def do_store(self, n0, w_value):
        pass
    def size(self):
        return self._size
    def generalized_strategy_for(self, w_val):
        return find_storage_for_objects(self.space, [w_val])
    @staticmethod
    def static_can_contain(space, w_val):
        return isinstance(w_val, model.W_Object) and w_val.is_nil(space)

class AbstractValueOrNilStorageMixin(object):
    # Class must provide: wrap, unwrap, nil_value, is_nil_value, wrapper_class
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']
    
    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self.storage = [self.nil_value] * size
    
    def size(self):
        return len(self.storage)
    
    def generalized_strategy_for(self, w_val):
        return ListStorageShadow
    
    def fetch(self, n0):
        val = self.storage[n0]
        if self.is_nil_value(val):
            return self.space.w_nil
        else:
            return self.wrap(self.space, val)
        
    def do_store(self, n0, w_val):
        if w_val.is_nil(self.space):
            self.storage[n0] = self.nil_value
        else:
            self.storage[n0] = self.unwrap(self.space, w_val)

# This is to avoid code duplication
@objectmodel.specialize.arg(0)
def _value_or_nil_can_handle(cls, space, w_val):
    return isinstance(w_val, model.W_Object) and w_val.is_nil(space) or \
            (isinstance(w_val, cls.wrapper_class) \
            and not cls.is_nil_value(cls.unwrap(space, w_val)))

class SmallIntegerOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "SmallIntegerOrNilStorageShadow"
    nil_value = constants.MAXINT
    wrapper_class = model.W_SmallInteger
    import_from_mixin(AbstractValueOrNilStorageMixin)
    
    @staticmethod
    def static_can_contain(space, w_val):
        return _value_or_nil_can_handle(SmallIntegerOrNilStorageShadow, space, w_val)
    @staticmethod
    def is_nil_value(val):
        return val == SmallIntegerOrNilStorageShadow.nil_value
    @staticmethod
    def wrap(space, val):
        return space.wrap_int(val)
    @staticmethod
    def unwrap(space, w_val):
        return space.unwrap_int(w_val)

class FloatOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "FloatOrNilStorageShadow"
    nil_value = sys.float_info.max
    wrapper_class = model.W_Float
    import_from_mixin(AbstractValueOrNilStorageMixin)
    
    @staticmethod
    def static_can_contain(space, w_val):
        return _value_or_nil_can_handle(FloatOrNilStorageShadow, space, w_val)
    @staticmethod
    def is_nil_value(val):
        return val == FloatOrNilStorageShadow.nil_value
    @staticmethod
    def wrap(space, val):
        return space.wrap_float(val)
    @staticmethod
    def unwrap(space, w_val):
        return space.unwrap_float(w_val)

def empty_storage(space, w_self, size, weak=False):
    if weak:
        return WeakListStorageShadow(space, w_self, size)
    if no_specialized_storage:
        return ListStorageShadow(space, w_self, size)
    return AllNilStorageShadow(space, w_self, size)

def find_storage_for_objects(space, vars, weak=False):
    if weak:
        return WeakListStorageShadow
    if no_specialized_storage:
        return ListStorageShadow
    specialized_strategies = 3
    all_nil_can_handle = True
    small_int_can_handle = True
    float_can_handle = True
    for w_obj in vars:
        if all_nil_can_handle and not AllNilStorageShadow.static_can_contain(space, w_obj):
            all_nil_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if small_int_can_handle and not SmallIntegerOrNilStorageShadow.static_can_contain(space, w_obj):
            small_int_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if float_can_handle and not FloatOrNilStorageShadow.static_can_contain(space, w_obj):
            float_can_handle = False
            specialized_strategies = specialized_strategies - 1
        
        if specialized_strategies <= 0:
            return ListStorageShadow
    
    if all_nil_can_handle:
        return AllNilStorageShadow
    if small_int_can_handle:
        return SmallIntegerOrNilStorageShadow
    if float_can_handle:
        return FloatOrNilStorageShadow
    
    # If this happens, please look for a bug in the code above.
    assert False, "No strategy could be found for list..."

class ListStorageMixin(object):
    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self.initialize_storage(size)
    def size(self):
        return len(self.storage)
    def copy_from(self, other_shadow):
        if self.size() != other_shadow.size():
            self.initialize_storage(other_shadow.size())
        AbstractShadow.copy_from(self, other_shadow)

class ListStorageShadow(AbstractStorageShadow):
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']
    repr_classname = "ListStorageShadow"
    import_from_mixin(ListStorageMixin)
    
    def initialize_storage(self, size):
        self.storage = [self.space.w_nil] * size
    def fetch(self, n0):
        return self.storage[n0]
    def store(self, n0, w_value):
        self.storage[n0] = w_value

class WeakListStorageShadow(AbstractStorageShadow):
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']
    repr_classname = "WeakListStorageShadow"
    import_from_mixin(ListStorageMixin)
    
    def initialize_storage(self, size):
        self.storage = [weakref.ref(self.space.w_nil)] * size
    def fetch(self, n0):
        weakobj = self.storage[n0]
        return weakobj() or self.space.w_nil
    def store(self, n0, w_value):
        assert w_value is not None
        self.storage[n0] = weakref.ref(w_value)
    
class AbstractCachingShadow(ListStorageShadow):
    _immutable_fields_ = ['version?']
    _attrs_ = ['version']
    repr_classname = "AbstractCachingShadow"
    import_from_mixin(version.VersionMixin)
    version = None
    
    def __init__(self, space, w_self):
        ListStorageShadow.__init__(self, space, w_self, 0)
        self.changed()

# ____________________________________________________________

POINTERS = 0
BYTES = 1
WORDS = 2
WEAK_POINTERS = 3
COMPILED_METHOD = 4
FLOAT = 5
LARGE_POSITIVE_INTEGER = 6

class MethodNotFound(error.SmalltalkException):
    pass

class ClassShadowError(error.SmalltalkException):
    pass

class ClassShadow(AbstractCachingShadow):
    """A shadow for Smalltalk objects that are classes
    (i.e. used as the class of another Smalltalk object).
    """

    _attrs_ = ["name", "_instance_size", "instance_varsized", "instance_kind",
                "_s_methoddict", "_s_superclass", "subclass_s"]
    name = '??? (incomplete class info)'
    _s_superclass = _s_methoddict = None
    provides_getname = True
    repr_classname = "ClassShadow"
    
    def __init__(self, space, w_self):
        self.subclass_s = {}
        AbstractCachingShadow.__init__(self, space, w_self)

    def store(self, n0, w_val):
        AbstractCachingShadow.store(self, n0, w_val)
        if n0 == constants.CLASS_SUPERCLASS_INDEX:
            self.store_w_superclass(w_val)
        elif n0 == constants.CLASS_METHODDICT_INDEX:
            self.store_w_methoddict(w_val)
        elif n0 == constants.CLASS_FORMAT_INDEX:
            # read and painfully decode the format
            assert isinstance(w_val, model.W_SmallInteger)
            classformat = self.space.unwrap_int(w_val)
            # The classformat in Squeak, as an integer value, is:
            #    <2 bits=instSize//64><5 bits=cClass><4 bits=instSpec>
            #                                    <6 bits=instSize\\64><1 bit=0>
            # In Slang the value is read directly as a boxed integer, so that
            # the code gets a "pointer" whose bits are set as above, but
            # shifted one bit to the left and with the lowest bit set to 1.
            
            # Compute the instance size (really the size, not the number of bytes)
            instsize_lo = (classformat >> 1) & 0x3F
            instsize_hi = (classformat >> (9 + 1)) & 0xC0
            self._instance_size = (instsize_lo | instsize_hi) - 1  # subtract hdr
            # decode the instSpec
            format = (classformat >> 7) & 15
            self.instance_varsized = format >= 2
            
            # In case of raised exception below.
            self.changed()
            
            if format < 4:
                self.instance_kind = POINTERS
            elif format == 4:
                self.instance_kind = WEAK_POINTERS
            elif format == 6:
                if self.space.w_Float.is_same_object(self.w_self()):
                    self.instance_kind = FLOAT
                else:
                    self.instance_kind = WORDS
                if self.instsize() != 0:
                    raise ClassShadowError("can't have both words and a non-zero "
                                           "base instance size")
            elif 8 <= format <= 11:
                if self.space.w_LargePositiveInteger.is_same_object(self.w_self()):
                    self.instance_kind = LARGE_POSITIVE_INTEGER
                else:
                    self.instance_kind = BYTES
                if self.instsize() != 0:
                    raise ClassShadowError("can't have both bytes and a non-zero "
                                           "base instance size")
            elif 12 <= format <= 15:
                self.instance_kind = COMPILED_METHOD
            else:
                raise ClassShadowError("unknown format %d" % (format,))
        else:
            if self._w_self.w_class == self.space.classtable["w_Metaclass"]:
                # In case of Metaclasses, the "instance" class is stored in the last field.
                if n0 == self.size() - 1 and isinstance(w_val, model.W_PointersObject):
                    cl_shadow = w_val.as_class_get_shadow(self.space)
                    self.name = "%s class" % cl_shadow.getname()
                else:
                    return
            elif n0 == constants.CLASS_NAME_INDEX:
                # In case of regular classes, the name is stored here.
                self.store_w_name(w_val)
            else:
                return
        # Some of the special info has changed -> Switch version.
        self.changed()
    
    def store_w_superclass(self, w_class):
        superclass = self._s_superclass
        if w_class is None or w_class.is_nil(self.space):
            if superclass: superclass.detach_s_class(self)
            self._s_superclass = None
        else:
            assert isinstance(w_class, model.W_PointersObject)
            s_new_superclass = w_class.as_class_get_shadow(self.space)
            if superclass is s_new_superclass:
                return
            if superclass: superclass.detach_s_class(self)
            self._s_superclass = s_new_superclass
            s_new_superclass.attach_s_class(self)

    def store_w_methoddict(self, w_methoddict):
        methoddict = self._s_methoddict
        if w_methoddict is None or w_methoddict.is_nil(self.space):
            if methoddict: methoddict.s_class = None
            self._s_methoddict = None
        else:
            assert isinstance(w_methoddict, model.W_PointersObject)
            s_new_methoddict = w_methoddict.as_methoddict_get_shadow(self.space)
            if methoddict is s_new_methoddict:
                return
            if methoddict: methoddict.s_class = None
            self._s_methoddict = s_new_methoddict
            self._s_methoddict.s_class = self
            
    def attach_s_class(self, s_other):
        self.subclass_s[s_other] = None

    def detach_s_class(self, s_other):
        del self.subclass_s[s_other]
    
    def store_w_name(self, w_name):
        if isinstance(w_name, model.W_BytesObject):
            self.name = w_name.as_string()
        else:
            self.name = None
    
    @jit.unroll_safe
    def flush_method_caches(self):
        look_in_shadow = self
        while look_in_shadow is not None:
            look_in_shadow.s_methoddict().sync_method_cache()
            look_in_shadow = look_in_shadow._s_superclass

    def new(self, extrasize=0):
        w_cls = self.w_self()
        if self.instance_kind == POINTERS:
            size = self.instsize() + extrasize
            w_new = model.W_PointersObject(self.space, w_cls, size)
        elif self.instance_kind == WORDS:
            w_new = model.W_WordsObject(self.space, w_cls, extrasize)
        elif self.instance_kind == BYTES:
            w_new = model.W_BytesObject(self.space, w_cls, extrasize)
        elif self.instance_kind == COMPILED_METHOD:
            w_new = model.W_CompiledMethod(self.space, extrasize)
        elif self.instance_kind == FLOAT:
            w_new = model.W_Float(0) # Squeak gives a random piece of memory
        elif self.instance_kind == LARGE_POSITIVE_INTEGER:
            if extrasize <= 4:
                w_new = model.W_LargePositiveInteger1Word(0, extrasize)
            else:
                w_new = model.W_BytesObject(self.space, w_cls, extrasize)
        elif self.instance_kind == WEAK_POINTERS:
            size = self.instsize() + extrasize
            w_new = model.W_PointersObject(self.space, w_cls, size, weak=True)
        else:
            raise NotImplementedError(self.instance_kind)
        return w_new

    def w_methoddict(self):
        return self._s_methoddict.w_self()

    def s_methoddict(self):
        return self._s_methoddict

    def s_superclass(self):
        return self._s_superclass

    def getname(self):
        return self.name

    # _______________________________________________________________
    # Methods for querying the format word, taken from the blue book:
    #
    # included so that we can reproduce code from the reference impl
    # more easily

    def ispointers(self):
        " True if instances of this class have data stored as pointers "
        XXX   # what about weak pointers?
        return self.format == POINTERS

    def iswords(self):
        " True if instances of this class have data stored as numerical words "
        XXX   # what about weak pointers?
        return self.format in (POINTERS, WORDS)

    def isbytes(self):
        " True if instances of this class have data stored as numerical bytes "
        return self.format == BYTES

    @constant_for_version
    def isvariable(self):
        " True if instances of this class have indexed inst variables "
        return self.instance_varsized

    @constant_for_version
    def instsize(self):
        " Number of named instance variables for each instance of this class "
        return self._instance_size

    # _______________________________________________________________
    # Other Methods

    @constant_for_version_arg
    def lookup(self, w_selector):
        look_in_shadow = self
        while look_in_shadow is not None:
            w_method = look_in_shadow.s_methoddict().find_selector(w_selector)
            if w_method is not None:
                # Old images don't store compiledin-info in literals.
                if not w_method.w_compiledin:
                    w_method.w_compiledin = look_in_shadow.w_self()
                    w_method.changed()
                return w_method
            look_in_shadow = look_in_shadow._s_superclass
        raise MethodNotFound(self, w_selector)

    def changed(self):
        self.superclass_changed(version.Version())

    # this is done, because the class-hierarchy contains cycles
    def superclass_changed(self, version):
        if self.version is not version:
            self.version = version
            for s_class in self.subclass_s:
                s_class.superclass_changed(version)
        
    # _______________________________________________________________
    # Methods used only in testing

    def inherits_from(self, s_superclass):
        "NOT_RPYTHON"     # this is only for testing.
        classshadow = self
        while classshadow is not None:
            if classshadow is s_superclass:
                return True
            classshadow = classshadow.s_superclass()
        else:
            return False

    def initialize_methoddict(self):
        "NOT_RPYTHON"     # this is only for testing.
        if self._s_methoddict is None:
            w_methoddict = model.W_PointersObject(self.space, None, 2)
            w_methoddict.store(self.space, 1, model.W_PointersObject(self.space, None, 0))
            self._s_methoddict = w_methoddict.as_methoddict_get_shadow(self.space)
            self.s_methoddict().sync_method_cache()
        self.s_methoddict().invalid = False

    def installmethod(self, w_selector, w_method):
        "NOT_RPYTHON"     # this is only for testing.
        assert not isinstance(w_selector, str)
        self.initialize_methoddict()
        self.s_methoddict().methoddict[w_selector] = w_method
        if isinstance(w_method, model.W_CompiledMethod):
            w_method.w_compiledin = self.w_self()

class MethodDictionaryShadow(ListStorageShadow):

    _immutable_fields_ = ['invalid?', 's_class']
    _attrs_ = ['methoddict', 'invalid', 's_class']
    repr_classname = "MethodDictionaryShadow"
    
    def __init__(self, space, w_self):
        self.invalid = True
        self.s_class = None
        self.methoddict = {}
        ListStorageShadow.__init__(self, space, w_self, 0)

    def attach_shadow(self):
        self.sync_method_cache()
        
    def update(self):
        self.sync_method_cache()
        
    def find_selector(self, w_selector):
        if self.invalid:
            return None # we may be invalid if Smalltalk code did not call flushCache
        return self.methoddict.get(w_selector, None)

    # Remove update call for changes to ourselves:
    # Whenever a method is added, it's keyword is added to w_self, then the
    # w_compiled_method is added to our observee.
    # sync_method_cache at this point would not have the desired effect, because in
    # the Smalltalk Implementation, the dictionary changes first. Afterwards
    # its contents array is filled with the value belonging to the new key.
    def store(self, n0, w_value):
        ListStorageShadow.store(self, n0, w_value)
        self.invalid = True

    def _as_md_entry(self, w_selector):
        if isinstance(w_selector, model.W_BytesObject):
            return w_selector.as_string()
        else:
            return "%r" % w_selector # use the pointer for this

    def sync_method_cache(self):
        if self.size() == 0:
            return
        w_values = self.fetch(constants.METHODDICT_VALUES_INDEX)
        assert isinstance(w_values, model.W_PointersObject)
        s_values = w_values.as_observed_get_shadow(self.space)
        s_values.notify(self)
        size = self.size() - constants.METHODDICT_NAMES_INDEX
        self.methoddict = {}
        for i in range(size):
            w_selector = self.w_self().fetch(self.space, constants.METHODDICT_NAMES_INDEX+i)
            if not w_selector.is_nil(self.space):
                if not isinstance(w_selector, model.W_BytesObject):
                    pass
                    # TODO: Check if there's more assumptions about this.
                    #       Putting any key in the methodDict and running with
                    #       perform is actually supported in Squeak
                    # raise ClassShadowError("bogus selector in method dict")
                w_compiledmethod = w_values.fetch(self.space, i)
                if not isinstance(w_compiledmethod, model.W_CompiledMethod):
                    raise ClassShadowError("The methoddict must contain "
                                       "CompiledMethods only, for now. "
                                       "If the value observed is nil, our "
                                       "invalidating mechanism may be broken.")
                selector = self._as_md_entry(w_selector)
                self.methoddict[w_selector] = w_compiledmethod
                w_compiledmethod._likely_methodname = selector
        if self.s_class:
            self.s_class.changed()
        self.invalid = False

class AbstractRedirectingShadow(AbstractShadow):
    _attrs_ = ['_w_self_size']
    repr_classname = "AbstractRedirectingShadow"
    
    def __init__(self, space, w_self):
        AbstractShadow.__init__(self, space, w_self)
        if w_self is not None:
            self._w_self_size = w_self.size()
        else:
            self._w_self_size = 0

    def size(self):
        return self._w_self_size

class ContextPartShadow(AbstractRedirectingShadow):

    __metaclass__ = extendabletype
    _attrs_ = ['_s_sender', '_pc', '_temps_and_stack',
            '_stack_ptr', 'instances_w']
    repr_classname = "ContextPartShadow"
    
    _virtualizable_ = [
        "_s_sender", "_pc",
        "_temps_and_stack[*]", "_stack_ptr",
        "_w_self", "_w_self_size"
    ]

    # ______________________________________________________________________
    # Initialization
    
    def __init__(self, space, w_self):
        self._s_sender = None
        AbstractRedirectingShadow.__init__(self, space, w_self)
        self.instances_w = {}

    def copy_field_from(self, n0, other_shadow):
        try:
            AbstractRedirectingShadow.copy_field_from(self, n0, other_shadow)
        except error.SenderChainManipulation, e:
            assert e.s_context == self
        
    def copy_from(self, other_shadow):
        # Some fields have to be initialized before the rest, to ensure correct initialization.
        privileged_fields = self.fields_to_copy_first()
        for n0 in privileged_fields:
            self.copy_field_from(n0, other_shadow)
        
        # Now the temp size will be known.
        self.init_stack_and_temps()
        
        for n0 in range(self.size()):
            if n0 not in privileged_fields:
                self.copy_field_from(n0, other_shadow)
        
    def fields_to_copy_first(self):
        return []
    
    # ______________________________________________________________________
    # Accessing object fields
    
    def fetch(self, n0):
        if n0 == constants.CTXPART_SENDER_INDEX:
            return self.w_sender()
        if n0 == constants.CTXPART_PC_INDEX:
            return self.wrap_pc()
        if n0 == constants.CTXPART_STACKP_INDEX:
            return self.wrap_stackpointer()
        if self.stackstart() <= n0 < self.external_stackpointer():
            temp_i = self.stackdepth() - (n0-self.stackstart()) - 1
            assert temp_i >= 0
            return self.peek(temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return self.space.w_nil
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")

    def store(self, n0, w_value):
        if n0 == constants.CTXPART_SENDER_INDEX:
            assert isinstance(w_value, model.W_PointersObject)
            if w_value.is_nil(self.space):
                self._s_sender = None
            else:
                self.store_s_sender(w_value.as_context_get_shadow(self.space))
            return
        if n0 == constants.CTXPART_PC_INDEX:
            return self.store_unwrap_pc(w_value)
        if n0 == constants.CTXPART_STACKP_INDEX:
            return self.unwrap_store_stackpointer(w_value)
        if self.stackstart() <= n0 < self.external_stackpointer(): # XXX can be simplified?
            temp_i = self.stackdepth() - (n0-self.stackstart()) - 1
            assert temp_i >= 0
            return self.set_top(w_value, temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")
    
    # === Sender ===
    
    def store_s_sender(self, s_sender):
        assert s_sender is None or isinstance(s_sender, ContextPartShadow)
        self._s_sender = s_sender
        raise error.SenderChainManipulation(self)
    
    def w_sender(self):
        if self._s_sender is None:
            return self.space.w_nil
        return self._s_sender.w_self()
    
    def s_sender(self):
        return self._s_sender
    
    # === Stack Pointer ===
    
    def unwrap_store_stackpointer(self, w_sp1):
        # the stackpointer in the W_PointersObject starts counting at the
        # tempframe start
        # Stackpointer from smalltalk world == stacksize in python world
        self.store_stackpointer(self.space.unwrap_int(w_sp1))

    def store_stackpointer(self, size):
        depth = self.stackdepth()
        if size < depth:
            # TODO Warn back to user
            assert size >= 0
            self.pop_n(depth - size)
        else:
            for i in range(depth, size):
                self.push(self.space.w_nil)

    def stackdepth(self):
        return rarithmetic.intmask(self._stack_ptr)
    
    def wrap_stackpointer(self):
        return self.space.wrap_int(self.stackdepth())

    # === Program Counter ===
        
    def store_unwrap_pc(self, w_pc):
        if w_pc.is_nil(self.space):
            self.store_pc(-1)
        else:
            pc = self.space.unwrap_int(w_pc)
            pc -= self.w_method().bytecodeoffset()
            pc -= 1
            self.store_pc(pc)

    def wrap_pc(self):
        pc = self.pc()
        if pc == -1:
            return self.space.w_nil
        else:
            pc += 1
            pc += self.w_method().bytecodeoffset()
            return self.space.wrap_int(pc)

    def pc(self):
        return self._pc

    def store_pc(self, newpc):
        assert newpc >= -1
        self._pc = newpc
    
    # === Subclassed accessors ===
    
    def s_home(self):
        raise NotImplementedError()

    def stackstart(self):
        raise NotImplementedError()

    def w_receiver(self):
        raise NotImplementedError()
    
    def w_method(self):
        raise NotImplementedError()
    
    def tempsize(self):
        raise NotImplementedError()
    
    def is_closure_context(self):
        raise NotImplementedError()
    
    # === Other properties of Contexts ===
    
    def mark_returned(self):
        self.store_pc(-1)
        try:
            self.store_s_sender(None)
        except error.SenderChainManipulation, e:
            assert self == e.s_context

    def is_returned(self):
        return self.pc() == -1 and self.w_sender().is_nil(self.space)

    def external_stackpointer(self):
        return self.stackdepth() + self.stackstart()
    
    def stackend(self):
        # XXX this is incorrect when there is subclassing
        return self._w_self_size
    
    def getbytecode(self):
        jit.promote(self._pc)
        assert self._pc >= 0
        bytecode = self.w_method().getbytecode(self._pc)
        currentBytecode = ord(bytecode)
        self._pc += 1
        return currentBytecode
    
    # ______________________________________________________________________
    # Temporary Variables
    #
    # Every context has it's own stack. BlockContexts share their temps with 
    # their home contexts. MethodContexts created from a BlockClosure get their
    # temps copied from the closure upon activation. Changes are not propagated back;
    # this is handled by the compiler by allocating an extra Array for temps.

    def gettemp(self, index):
        raise NotImplementedError()

    def settemp(self, index, w_value):
        raise NotImplementedError()
    
    # ______________________________________________________________________
    # Stack Manipulation

    @jit.unroll_safe
    def init_stack_and_temps(self):
        stacksize = self.stackend() - self.stackstart()
        tempsize = self.tempsize()
        temps_and_stack = [None] * (stacksize + tempsize)
        self._temps_and_stack = temps_and_stack
        make_sure_not_resized(temps_and_stack)
        for i in range(tempsize):
            temps_and_stack[i] = self.space.w_nil
        self._stack_ptr = rarithmetic.r_uint(tempsize) # we point after the last element
    
    def stack_get(self, index0):
        return self._temps_and_stack[index0]
    
    def stack_put(self, index0, w_val):
        self._temps_and_stack[index0] = w_val
    
    def stack(self):
        """NOT_RPYTHON""" # purely for testing
        return self._temps_and_stack[self.tempsize():self._stack_ptr]

    def pop(self):
        #assert self._stack_ptr > self.tempsize()
        ptr = jit.promote(self._stack_ptr) - 1
        ret = self.stack_get(ptr)   # you get OverflowError if the stack is empty
        self.stack_put(ptr, None)
        self._stack_ptr = ptr
        return ret

    def push(self, w_v):
        #assert self._stack_ptr >= self.tempsize()
        #assert self._stack_ptr < self.stackend() - self.stackstart() + self.tempsize()
        ptr = jit.promote(self._stack_ptr)
        self.stack_put(ptr, w_v)
        self._stack_ptr = ptr + 1

    @jit.unroll_safe
    def push_all(self, lst):
        for elt in lst:
            self.push(elt)

    def top(self):
        return self.peek(0)

    def set_top(self, value, position=0):
        rpos = rarithmetic.r_uint(position)
        ptr = self._stack_ptr + ~rpos
        self.stack_put(ptr, value)

    def peek(self, idx):
        rpos = rarithmetic.r_uint(idx)
        ptr = jit.promote(self._stack_ptr) + ~rpos
        return self.stack_get(ptr)

    @jit.unroll_safe
    def pop_n(self, n):
        #assert n == 0 or self._stack_ptr - n >= self.tempsize()
        jit.promote(self._stack_ptr)
        while n > 0:
            n -= 1
            self._stack_ptr -= 1
            self.stack_put(self._stack_ptr, None)

    @jit.unroll_safe
    def pop_and_return_n(self, n):
        result = [self.peek(i) for i in range(n - 1, -1, -1)]
        self.pop_n(n)
        return result

    # ______________________________________________________________________
    # Primitive support
    
    def store_instances_array(self, w_class, match_w):
        # used for primitives 77 & 78
        self.instances_w[w_class] = match_w

    @jit.elidable
    def instances_array(self, w_class):
        return self.instances_w.get(w_class, None)

    # ______________________________________________________________________
    # Printing

    def print_stack(self, method=True):
        return self.print_padded_stack(method)[1]

    def print_padded_stack(self, method):
        padding = ret_str = ''
        if self.s_sender() is not None:
                padding, ret_str = self.s_sender().print_padded_stack(method)
        if method:
            desc = self.method_str()
        else:
            desc = self.short_str()
        return padding + ' ', '%s\n%s%s' % (ret_str, padding, desc)


class BlockContextShadow(ContextPartShadow):
    _attrs_ = ['_w_home', '_initialip', '_eargc']
    repr_classname = "BlockContextShadow"
    
    # === Initialization ===
    
    def __init__(self, space, w_self=None, w_home=None, argcnt=0, initialip=0):
        self = jit.hint(self, access_directly=True, fresh_virtualizable=True)
        creating_w_self = w_self is None
        if creating_w_self:
            s_home = w_home.as_methodcontext_get_shadow(space)
            contextsize = s_home.size() - s_home.tempsize()
            w_self = model.W_PointersObject(space, space.w_BlockContext, contextsize)
        ContextPartShadow.__init__(self, space, w_self)
        if creating_w_self:
            w_self.store_shadow(self)
        self.store_expected_argument_count(argcnt)
        self.store_initialip(initialip)
        if w_home:
            self.store_w_home(w_home)
        self.store_pc(initialip)
        self.init_stack_and_temps()

    def fields_to_copy_first(self):
        return [ constants.BLKCTX_HOME_INDEX ]
        
    # === Implemented accessors ===
        
    def s_home(self):
        return self._w_home.as_methodcontext_get_shadow(self.space)
        
    def stackstart(self):
        return constants.BLKCTX_STACK_START

    def tempsize(self):
        # A blockcontext doesn't have any temps
        return 0
        
    def w_receiver(self):
        return self.s_home().w_receiver()
        
    def w_method(self):
        retval = self.s_home().w_method()
        assert isinstance(retval, model.W_CompiledMethod)
        return retval
    
    def is_closure_context(self):
        return True
    
    # === Temporary variables ===
    
    def gettemp(self, index):
        return self.s_home().gettemp(index)

    def settemp(self, index, w_value):
        self.s_home().settemp(index, w_value)
    
    # === Accessing object fields ===
    
    def fetch(self, n0):
        if n0 == constants.BLKCTX_HOME_INDEX:
            return self._w_home
        if n0 == constants.BLKCTX_INITIAL_IP_INDEX:
            return self.wrap_initialip()
        if n0 == constants.BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX:
            return self.wrap_eargc()
        else:
            return ContextPartShadow.fetch(self, n0)

    def store(self, n0, w_value):
        if n0 == constants.BLKCTX_HOME_INDEX:
            return self.store_w_home(w_value)
        if n0 == constants.BLKCTX_INITIAL_IP_INDEX:
            return self.unwrap_store_initialip(w_value)
        if n0 == constants.BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX:
            return self.unwrap_store_eargc(w_value)
        else:
            return ContextPartShadow.store(self, n0, w_value)
    
    def store_w_home(self, w_home):
        assert isinstance(w_home, model.W_PointersObject)
        self._w_home = w_home
    
    def unwrap_store_initialip(self, w_value):
        initialip = self.space.unwrap_int(w_value)
        initialip -= 1 + self.w_method().literalsize
        self.store_initialip(initialip)

    def store_initialip(self, initialip):
        self._initialip = initialip
        
    def wrap_initialip(self):
        initialip = self.initialip()
        initialip += 1 + self.w_method().literalsize
        return self.space.wrap_int(initialip)

    def initialip(self):
        return self._initialip
        
    def unwrap_store_eargc(self, w_value):
        self.store_expected_argument_count(self.space.unwrap_int(w_value))

    def wrap_eargc(self):
        return self.space.wrap_int(self.expected_argument_count())

    def expected_argument_count(self):
        return self._eargc

    def store_expected_argument_count(self, argc):
        self._eargc = argc

    # === Stack Manipulation ===
    
    def reset_stack(self):
        self.pop_n(self.stackdepth())

    # === Printing ===
    
    def short_str(self):
        return 'BlockContext of %s (%s) [%d]' % (
            self.w_method().get_identifier_string(),
            self.w_receiver().as_repr_string(),
            self.pc() + 1
        )

    def method_str(self):
        return '[] of %s' % self.w_method().get_identifier_string()

class MethodContextShadow(ContextPartShadow):
    _attrs_ = ['closure', '_w_receiver', '_w_method']
    repr_classname = "MethodContextShadow"
    
    # === Initialization ===
    
    @jit.unroll_safe
    def __init__(self, space, w_self=None, w_method=None, w_receiver=None,
                              arguments=None, s_sender=None, closure=None, pc=0):
        self = jit.hint(self, access_directly=True, fresh_virtualizable=True)
        ContextPartShadow.__init__(self, space, w_self)
        self.store_w_receiver(w_receiver)
        self.store_pc(pc)
        self.closure = closure
        
        if w_method:
            self.store_w_method(w_method)
            # The summand is needed, because we calculate i.a. our stackdepth relative of the size of w_self.
            size = w_method.compute_frame_size() + self.space.w_MethodContext.as_class_get_shadow(self.space).instsize()
            self._w_self_size = size
            self.init_stack_and_temps()
        else:
            self._w_method = None
        
        if s_sender:
            try:
                self.store_s_sender(s_sender)
            except error.SenderChainManipulation, e:
                assert self == e.s_context
        
        if arguments:
            argc = len(arguments)
            for i0 in range(argc):
                self.settemp(i0, arguments[i0])
        else:
            argc = 0
        
        if closure:
            for i0 in range(closure.size()):
                self.settemp(i0+argc, closure.at0(i0))

    def fields_to_copy_first(self):
        return [ constants.MTHDCTX_METHOD, constants.MTHDCTX_CLOSURE_OR_NIL ]
    
    # === Accessing object fields ===
    
    def fetch(self, n0):
        if n0 == constants.MTHDCTX_METHOD:
            return self.w_method()
        if n0 == constants.MTHDCTX_CLOSURE_OR_NIL:
            if self.closure:
                return self.closure._w_self
            else:
                return self.space.w_nil
        if n0 == constants.MTHDCTX_RECEIVER:
            return self.w_receiver()
        temp_i = n0-constants.MTHDCTX_TEMP_FRAME_START
        if (0 <= temp_i < self.tempsize()):
            return self.gettemp(temp_i)
        else:
            return ContextPartShadow.fetch(self, n0)

    def store(self, n0, w_value):
        if n0 == constants.MTHDCTX_METHOD:
            return self.store_w_method(w_value)
        if n0 == constants.MTHDCTX_CLOSURE_OR_NIL:
            if w_value.is_nil(self.space):
                self.closure = None
            else:
                self.closure = wrapper.BlockClosureWrapper(self.space, w_value)
            return
        if n0 == constants.MTHDCTX_RECEIVER:
            self.store_w_receiver(w_value)
            return
        temp_i = n0-constants.MTHDCTX_TEMP_FRAME_START
        if (0 <=  temp_i < self.tempsize()):
            return self.settemp(temp_i, w_value)
        else:
            return ContextPartShadow.store(self, n0, w_value)
    
    def store_w_receiver(self, w_receiver):
        self._w_receiver = w_receiver
    
    # === Implemented Accessors ===
    
    def s_home(self):
        if self.is_closure_context():
            # this is a context for a blockClosure
            w_outerContext = self.closure.outerContext()
            assert isinstance(w_outerContext, model.W_PointersObject)
            s_outerContext = w_outerContext.as_context_get_shadow(self.space)
            # XXX check whether we can actually return from that context
            if s_outerContext.is_returned():
                raise error.BlockCannotReturnError()
            return s_outerContext.s_home()
        else:
            return self
    
    def stackstart(self):
        return constants.MTHDCTX_TEMP_FRAME_START
        
    def store_w_method(self, w_method):
        assert isinstance(w_method, model.W_CompiledMethod)
        self._w_method = w_method

    def w_receiver(self):
        return self._w_receiver
        
    def w_method(self):
        retval = self._w_method
        assert isinstance(retval, model.W_CompiledMethod)
        return retval
    
    def tempsize(self):
        if not self.is_closure_context():
            return self.w_method().tempsize()
        else:
            return self.closure.tempsize()
    
    def is_closure_context(self):
        return self.closure is not None
    
    # ______________________________________________________________________
    # Marriage of MethodContextShadows with PointerObjects only when required

    def w_self(self):
        if self._w_self is not None:
            return self._w_self
        else:
            s_MethodContext = self.space.w_MethodContext.as_class_get_shadow(self.space)
            size = self.size() - s_MethodContext.instsize()
            space = self.space
            w_self = s_MethodContext.new(size)
            assert isinstance(w_self, model.W_PointersObject)
            w_self.store_shadow(self)
            self._w_self = w_self
            self._w_self_size = w_self.size()
            return w_self
    
    # === Temporary variables ===
    
    def gettemp(self, index0):
        return self.stack_get(index0)

    def settemp(self, index0, w_value):
        self.stack_put(index0, w_value)

    # === Printing ===

    def __str__(self):
        retval = '\nMethodContext of:'
        retval += self.w_method().as_string(markBytecode=self.pc() + 1)
        retval += "Stackptr: %d (this is an empty ascending stack with args and temps (%d), then stack)" % (self._stack_ptr, self.tempsize())
        retval += "\nStack   : " + str(self._temps_and_stack[:self._stack_ptr])
        return retval

    def short_str(self):
        method_str = self.method_str()
        argcount = method_str.count(':')
        if argcount == 0:
            return '%s (rcvr: %s) [pc: %d]' % (
                method_str,
                self.w_receiver().as_repr_string(),
                self.pc() + 1
            )
        args = '%d' % argcount
        for i in range(argcount):
            args += ': %s' % self.peek(argcount -1 - i).as_repr_string()
        return '%s (rcvr: %s) [pc: %d] (%s)' % (
            self.method_str(),
            self.w_receiver().as_repr_string(),
            self.pc() + 1,
            args
        )

    def method_str(self):
        block = '[] of ' if self.is_closure_context() else ''
        return '%s%s' % (block, self.w_method().get_identifier_string())

class CachedObjectShadow(AbstractCachingShadow):
    repr_classname = "CachedObjectShadow"

    @elidable_for_version
    def fetch(self, n0):
        return AbstractCachingShadow.fetch(self, n0)

    def store(self, n0, w_value):
        AbstractCachingShadow.store(self, n0, w_value)
        self.changed()

class ObserveeShadow(ListStorageShadow):
    _attrs_ = ['dependent']
    repr_classname = "ObserveeShadow"
    def __init__(self, space, w_self):
        ListStorageShadow.__init__(self, space, w_self, 0)
        self.dependent = None

    def store(self, n0, w_value):
        ListStorageShadow.store(self, n0, w_value)
        if self.dependent:
            self.dependent.update()

    def notify(self, dependent):
        if self.dependent is not None and dependent is not self.dependent:
            raise RuntimeError('Meant to be observed by only one value, so far')
        self.dependent = dependent
