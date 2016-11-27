from rsqueakvm import constants, wrapper, display, storage
from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, SYSTEM_ATTRIBUTE_IMAGE_ARGS_INDEX, IS_64BIT
from rsqueakvm.error import WrappingError, UnwrappingError
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_LargeIntegerWord, W_LargeIntegerBig
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.block_closure import W_BlockClosure
from rsqueakvm.util.version import Version

from rpython.rlib import jit, rbigint, rarithmetic
from rpython.rlib.objectmodel import instantiate, specialize, import_from_mixin, we_are_translated, always_inline
from rpython.rlib.rarithmetic import intmask, r_uint, r_uint32, int_between, is_valid_int, r_ulonglong, r_longlong, r_int64


class ConstantMixin(object):
    """Mixin for constant values that can be edited, but will be promoted
    to a constant when jitting."""
    _immutable_fields_ = ["value?"]

    def __init__(self, initial_value=None):
        if initial_value is None:
            initial_value = self.default_value
        self.value = initial_value

    def set(self, value):
        self.value = value

    def get(self):
        return self.value

class ConstantFlag(object):
    import_from_mixin(ConstantMixin)
    default_value = False
    def is_set(self):
        return self.get()
    def activate(self):
        self.set(True)
    def deactivate(self):
        self.set(False)

class ConstantString(object):
    import_from_mixin(ConstantMixin)
    default_value = ""

class ConstantObject(object):
    import_from_mixin(ConstantMixin)
    default_value = None

class ConstantVersion(object):
    import_from_mixin(ConstantMixin)
    default_value = Version()

def empty_object():
    return instantiate(W_PointersObject)

class ForceHeadless(object):
    def __init__(self, space):
        self.space = space
        self.was_headfull = not space.headless.is_set()
    def __enter__(self):
        if self.was_headfull:
            self.space.headless.activate()
    def __exit__(self, type, value, traceback):
        if self.was_headfull:
            self.space.headless.deactivate()

class ObjSpace(object):
    _immutable_fields_ = ['objtable']

    def __init__(self):
        # This is a hack; see compile_code() in main.py
        self.suppress_process_switch = ConstantFlag()
        self.run_spy_hacks = ConstantFlag()
        self.headless = ConstantFlag()
        self.highdpi = ConstantFlag(True)
        self.software_renderer = ConstantFlag(False)
        self.no_display = ConstantFlag(False)
        self.silent = ConstantFlag(False)
        self.omit_printing_raw_bytes = ConstantFlag()
        self.image_loaded = ConstantFlag()
        self.is_spur = ConstantFlag()
        self.uses_block_contexts = ConstantFlag()
        self.simulate_numeric_primitives = ConstantFlag()

        self.classtable = {}
        self.objtable = {}
        self.system_attributes = {}
        self._system_attribute_version = ConstantVersion()
        self._executable_path = ConstantString()
        self.title = ConstantString()
        self.altf4quit = ConstantFlag()
        self._display = ConstantObject()

        # Create the nil object.
        # Circumvent the constructor because nil is already referenced there.
        w_nil = empty_object()
        self.add_bootstrap_object("w_nil", w_nil)

        self.strategy_factory = storage.StrategyFactory(self)
        self.make_bootstrap_classes()
        self.make_bootstrap_objects()

    def runtime_setup(self, exepath, argv, image_name, image_args_idx):
        fullpath = exepath
        self._executable_path.set(fullpath)
        for i in range(image_args_idx, len(argv)):
            self.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_ARGS_INDEX + i - image_args_idx, argv[i])
        self.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, image_name)
        self.image_loaded.activate()
        self.init_system_attributes(argv)
        from rsqueakvm.plugins.plugin import PluginStartupScripts
        for func in PluginStartupScripts:
            func(self, argv)

    def init_system_attributes(self, argv):
        for i in xrange(1, len(argv)):
            self.set_system_attribute(-i, argv[i])
        import platform
        from rsqueakvm.main import VERSION, BUILD_DATE
        self.set_system_attribute(0, self._executable_path.get())
        self.set_system_attribute(1001, platform.system())    # operating system
        self.set_system_attribute(1002, platform.version())   # operating system version
        self.set_system_attribute(1003, platform.processor())  # platform's processor type
        self.set_system_attribute(1004, VERSION)
        self.set_system_attribute(1006, BUILD_DATE)
        self.set_system_attribute(1007, "rsqueak")            # interpreter class (invented for Cog)

    def get_system_attribute(self, idx):
        return self._pure_get_system_attribute(idx, self._system_attribute_version.get())

    @jit.elidable
    def _pure_get_system_attribute(self, idx, version):
        return self.system_attributes[idx]

    def set_system_attribute(self, idx, value):
        self.system_attributes[idx] = value
        self._system_attribute_version.set(Version())

    def populate_special_objects(self, specials):
        for name, idx in constants.objects_in_special_object_table.items():
            name = "w_" + name
            if not name in self.objtable or not self.objtable[name]:
                try:
                    self.objtable[name] = specials[idx]
                except IndexError:
                    # if it's not yet in the table, the interpreter has to fill
                    # the gap later in populate_remaining_special_objects
                    self.objtable[name] = None
        self.classtable["w_Metaclass"] = self.w_SmallInteger.getclass(self).getclass(self)

    def add_bootstrap_class(self, name, cls):
        self.classtable[name] = cls
        setattr(self, name, cls)

    def make_bootstrap_classes(self):
        names = [ "w_" + name for name in constants.classes_in_special_object_table.keys() ]
        for name in names:
            cls = empty_object()
            self.add_bootstrap_class(name, cls)

    def add_bootstrap_object(self, name, obj):
        self.objtable[name] = obj
        setattr(self, name, obj)

    def make_bootstrap_object(self, name):
        obj = empty_object()
        self.add_bootstrap_object(name, obj)

    def make_bootstrap_objects(self):
        self.make_bootstrap_object("w_true")
        self.make_bootstrap_object("w_false")
        self.make_bootstrap_object("w_special_selectors")
        self.add_bootstrap_object("w_minus_one", W_SmallInteger(-1))
        self.add_bootstrap_object("w_zero", W_SmallInteger(0))
        self.add_bootstrap_object("w_one", W_SmallInteger(1))
        self.add_bootstrap_object("w_two", W_SmallInteger(2))

        # Certain special objects are already created. The rest will be
        # populated when the image is loaded, but prepare empty slots for them.
        for name in constants.objects_in_special_object_table:
            name = "w_" + name
            if not name in self.objtable:
                self.add_bootstrap_object(name, None)

    @jit.elidable
    def special_object(self, which):
        return self.objtable[which]

    # ============= Methods for wrapping and unwrapping stuff =============

    @specialize.argtype(1)
    def wrap_int(self, val):
        if isinstance(val, rbigint.rbigint):
            return self.wrap_rbigint(val)
        elif isinstance(val, int):
            return self.wrap_smallint_unsafe(val)
        elif isinstance(val, r_uint):
            if val <= r_uint(constants.MAXINT):
                return self.wrap_smallint_unsafe(intmask(val))
            else:
                return self.wrap_wordint_direct(val, self.w_LargePositiveInteger)
        elif IS_64BIT and isinstance(val, r_uint32):
            return self.wrap_smallint_unsafe(intmask(val))
        elif isinstance(val, r_longlong) or isinstance(val, r_int64):
            # use '&' instead of 'and' in these checks, so we only generate 1 guard instead of two
            if (constants.MININT <= val) & (val <= constants.MAXINT):
                return self.wrap_smallint_unsafe(intmask(val))
            elif (0 <= val) & (val <= r_longlong(constants.U_MAXINT)):
                return self.wrap_wordint_direct(r_uint(val), self.w_LargePositiveInteger)
            elif (0 > val) & (-r_longlong(constants.U_MAXINT) <= val):
                return self.wrap_wordint_direct(r_uint(val), self.w_LargeNegativeInteger)
            else:
                return self.wrap_rbigint_direct(rbigint.rbigint.fromrarith_int(val))
        elif isinstance(val, r_ulonglong):
            if val <= r_ulonglong(constants.MAXINT):
                return self.wrap_smallint_unsafe(intmask(val))
            elif val <= constants.U_MAXINT:
                return self.wrap_wordint_direct(r_uint(val), self.w_LargePositiveInteger)
            else:
                return self.wrap_rbigint_direct(rbigint.rbigint.fromrarith_int(val))
        else:
            raise WrappingError

    def wrap_smallint_unsafe(self, val):
        assert is_valid_int(val)
        return W_SmallInteger(intmask(val))

    def wrap_rbigint(self, val):
        if val.sign >= 0:
            w_class = self.w_LargePositiveInteger
        else:
            w_class = self.w_LargeNegativeInteger
        # the logic below is an copied from rbigint.toint to make the guards inline better
        if val.numdigits() <= rbigint.MAX_DIGITS_THAT_CAN_FIT_IN_INT:
            try:
                uint = val._touint_helper() # this doesn't check the sign, which we want
            except OverflowError:
                return self.wrap_rbigint_direct(val, w_class)
            if val.sign >= 0:
                res = intmask(uint)
                if res >= 0:
                    return self.wrap_smallint_unsafe(res)
            else:
                res = intmask(-uint)
                if res < 0:
                    return self.wrap_smallint_unsafe(res)
            return self.wrap_wordint_direct(uint, w_class)
        return self.wrap_rbigint_direct(val, w_class)

    @always_inline
    def wrap_rbigint_direct(self, val, w_class=None):
        if w_class is None:
            if val.sign >= 0:
                w_class = self.w_LargePositiveInteger
            else:
                w_class = self.w_LargeNegativeInteger
        return W_LargeIntegerBig(self, w_class, val, 0)

    def wrap_wordint_direct(self, val, w_class):
        return W_LargeIntegerWord(self, w_class, val, constants.BYTES_PER_MACHINE_INT)

    def wrap_float(self, i):
        return W_Float(i)

    def wrap_string(self, string):
        w_inst = self.w_String.as_class_get_shadow(self).new(len(string))
        assert isinstance(w_inst, W_BytesObject)
        w_inst.setbytes(list(string))
        return w_inst

    def wrap_char(self, c):
        # return self.w_charactertable.fetch(self, ord(c))
        return W_Character(ord(c))

    def wrap_bool(self, b):
        if b:
            return self.w_true
        else:
            return self.w_false

    def wrap_list(self, lst_w):
        """
        Converts a Python list of wrapped objects into
        a wrapped smalltalk array
        """
        lstlen = len(lst_w)
        res = self.w_Array.as_class_get_shadow(self).new(lstlen)
        for i in range(lstlen):
            res.atput0(self, i, lst_w[i])
        return res

    @jit.unroll_safe
    def wrap_list_unroll_safe(self, lst_w):
        lstlen = len(lst_w)
        res = self.w_Array.as_class_get_shadow(self).new(lstlen)
        for i in range(lstlen):
            res.atput0(self, i, lst_w[i])
        return res

    def unwrap_int(self, w_value):
        return w_value.unwrap_int(self)

    def unwrap_uint(self, w_value):
        return w_value.unwrap_uint(self)

    def unwrap_positive_uint(self, w_value):
        if w_value.is_positive(self):
            return w_value.unwrap_uint(self)
        raise UnwrappingError

    def unwrap_int64(self, w_value):
        if IS_64BIT:
            return w_value.unwrap_int(self)
        else:
            return w_value.unwrap_int64(self)

    def unwrap_rbigint(self, w_value):
        return w_value.unwrap_rbigint(self)

    def unwrap_char_as_byte(self, w_char):
        return w_char.unwrap_char_as_byte(self)

    def unwrap_float(self, w_v):
        return w_v.unwrap_float(self)

    def unwrap_array(self, w_array):
        return w_array.unwrap_array(self)

    def unwrap_string(self, w_object):
        return w_object.unwrap_string(self)

    # ============= Access to static information =============

    @specialize.arg(1)
    def get_special_selector(self, selector):
        i0 = constants.find_selectorindex(selector)
        self.w_special_selectors.as_cached_object_get_shadow(self)
        return self.w_special_selectors.fetch(self, i0)

    def executable_path(self):
        return self._executable_path.get()

    def window_title(self):
        title = self.title.get()
        imgpath = self.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX)
        if len(title) == 0:
            title = "RSqueak (%s)" % imgpath
        else:
            title = "%s (%s)" % (self.title.get(), imgpath)
        return title

    def display(self):
        disp = self._display.get()
        if disp is None:
            # Create lazy to allow headless execution.
            if self.no_display.is_set():
                disp = display.NullDisplay()
                print 'Attaching a dummy display...'
            else:
                disp = display.SDLDisplay(
                    self.window_title(),
                    self.highdpi.is_set(),
                    self.software_renderer.is_set(),
                    self.altf4quit.is_set()
                )
            self._display.set(disp)
        return jit.promote(disp)

    def smalltalk_at(self, string):
        """A helper to find a class by name in modern Squeak images"""
        w_sd = self.special_object("w_smalltalkdict")
        if w_sd.instsize() == 1:
            w_globals = w_sd.fetch(self, 0)
            if w_globals.instsize() == 6:
                w_bindings = w_globals.fetch(self, 2)
                if w_bindings.instsize() == 2:
                    w_array = w_bindings.fetch(self, 1)
                    size = w_array.varsize()
                    for i in range(size):
                        w_assoc = w_array.fetch(self, i)
                        if w_assoc.instsize() == 2:
                            try:
                                if self.unwrap_string(w_assoc.fetch(self, 0)) == string:
                                    return w_assoc.fetch(self, 1)
                            except UnwrappingError:
                                pass
        elif w_sd.instsize() == 2:
            w_array = w_sd.fetch(self, 1)
            size = w_array.varsize()
            for i in range(size):
                w_assoc = w_array.fetch(self, i)
                if w_assoc.instsize() == 2:
                    try:
                        if self.unwrap_string(w_assoc.fetch(self, 0)) == string:
                            return w_assoc.fetch(self, 1)
                    except UnwrappingError:
                        pass

    # ============= Other Methods =============

    def _freeze_(self):
        return True

    @jit.unroll_safe
    def newClosure(self, w_outer_ctxt, pc, numArgs, copiedValues):
        assert isinstance(w_outer_ctxt, W_PointersObject)
        w_method = w_outer_ctxt.as_context_get_shadow(self).w_method()
        pc_with_bytecodeoffset = pc + w_method.bytecodeoffset() + 1
        numCopied = len(copiedValues)
        w_closure = W_BlockClosure(self, w_outer_ctxt, pc_with_bytecodeoffset, numArgs, numCopied)
        for i0 in range(numCopied):
            w_closure.atput0(self, i0, copiedValues[i0])
        return w_closure
