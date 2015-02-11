
from spyvm import model, constants, error, wrapper
from spyvm.storage import AbstractCachingShadow, ListStorageShadow
from spyvm.util.version import constant_for_version, constant_for_version_arg, Version
from rpython.rlib import jit

POINTERS = 0
BYTES = 1
WORDS = 2
WEAK_POINTERS = 3
COMPILED_METHOD = 4
FLOAT = 5
LARGE_POSITIVE_INTEGER = 6

class ClassShadowError(error.SmalltalkException):
    exception_type = "ClassShadowError"

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

    def __init__(self, space, w_self, size):
        self.subclass_s = {}
        AbstractCachingShadow.__init__(self, space, w_self, size)

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
            self.store_s_methoddict(s_new_methoddict)

    def store_s_methoddict(self, s_methoddict):
        assert isinstance(s_methoddict, MethodDictionaryShadow)
        s_methoddict.s_class = self
        s_methoddict.sync_method_cache()
        self._s_methoddict = s_methoddict

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
            look_in_shadow.s_methoddict().flush_method_cache()
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
                return w_method
            look_in_shadow = look_in_shadow._s_superclass
        raise error.MethodNotFound()

    def changed(self):
        self.superclass_changed(Version())

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
            w_methoddict.store(self.space, constants.METHODDICT_VALUES_INDEX, model.W_PointersObject(self.space, None, 0))
            self.store_s_methoddict(w_methoddict.as_methoddict_get_shadow(self.space))

    def installmethod(self, w_selector, w_method):
        "NOT_RPYTHON"     # this is only for testing.
        assert not isinstance(w_selector, str)
        self.initialize_methoddict()
        self.s_methoddict().methoddict[w_selector] = w_method
        if isinstance(w_method, model.W_CompiledMethod):
            w_method.compiledin_class = self.w_self()

class MethodDictionaryShadow(ListStorageShadow):

    _immutable_fields_ = ['s_class']
    _attrs_ = ['methoddict', 's_class']
    repr_classname = "MethodDictionaryShadow"

    def __init__(self, space, w_self, size):
        self.s_class = None
        self.methoddict = {}
        ListStorageShadow.__init__(self, space, w_self, size)

    def handle_become(self, s_other):
        self_s_class, other_s_class = self.s_class, s_other.s_class
        if self_s_class:
            self_s_class.store_s_methoddict(s_other)
        if other_s_class:
            other_s_class.store_s_methoddict(self)

    def update(self):
        self.sync_method_cache()

    def find_selector(self, w_selector):
        return self.methoddict.get(w_selector, None)

    # We do not call update() after changes to ourselves:
    # Whenever a method is added, it's keyword is added to w_self, then the
    # w_compiled_method is added to our observee.
    # sync_method_cache at this point would not have the desired effect, because in
    # the Smalltalk Implementation, the dictionary changes first. Afterwards
    # its contents array is filled with the value belonging to the new key.
    def store(self, n0, w_value):
        ListStorageShadow.store(self, n0, w_value)
        if n0 == constants.METHODDICT_VALUES_INDEX:
            self.setup_notification()
        if n0 >= constants.METHODDICT_NAMES_INDEX:
            # the caller / user cannot be expected to add the compiledMethod to the observee next (as indicated above)
            # in case of clone / copyFrom the compiledMethod is already contained, so a sync is necessary here too
            self.sync_method_cache()
    
    def setup_notification(self):
        self.w_values().as_observed_get_shadow(self.space).notify(self)
        
    def w_values(self):
        w_values = self.fetch(constants.METHODDICT_VALUES_INDEX)
        assert isinstance(w_values, model.W_PointersObject)
        return w_values

    def flush_method_cache(self):
        self.sync_method_cache()

    def sync_method_cache(self):
        if self.size() == 0:
            return
        self.methoddict = {}
        size = self.size() - constants.METHODDICT_NAMES_INDEX
        w_values = self.w_values()
        for i in range(size):
            w_selector = self.w_self().fetch(self.space, constants.METHODDICT_NAMES_INDEX+i)
            if not w_selector.is_nil(self.space):
                if isinstance(w_selector, model.W_BytesObject):
                    selector = w_selector.as_string()
                else:
                    selector = "? (non-byteobject selector)"
                    pass
                    # TODO: Check if there's more assumptions about this.
                    #       Putting any key in the methodDict and running with
                    #       perform is actually supported in Squeak
                    # raise ClassShadowError("bogus selector in method dict")

                w_compiledmethod = w_values.fetch(self.space, i)
                if not isinstance(w_compiledmethod, model.W_Object):
                    raise ClassShadowError("The methoddict must contain only wrapped Objects."
                                       "If the value observed is nil, our "
                                       "invalidating mechanism may be broken.")
                self.methoddict[w_selector] = w_compiledmethod
                if isinstance(w_compiledmethod, model.W_CompiledMethod) and self.s_class:
                    if (w_compiledmethod.lookup_class is not self.s_class.w_self() or
                        w_compiledmethod.lookup_selector is not selector):
                        w_compiledmethod.set_lookup_class_and_name(self.s_class.w_self(), selector)
        if self.s_class:
            self.s_class.changed()
