from rsqueakvm import constants, display, storage
from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, SYSTEM_ATTRIBUTE_IMAGE_ARGS_INDEX, IS_64BIT
from rsqueakvm.error import WrappingError, UnwrappingError
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_LargeIntegerWord, W_LargeIntegerBig
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.block_closure import W_BlockClosure
from rsqueakvm.util.version import Version
from rsqueakvm.util.cells import QuasiConstant

from rpython.rlib import jit, rbigint
from rpython.rlib.objectmodel import instantiate, specialize, import_from_mixin, we_are_translated, always_inline
from rpython.rlib.rarithmetic import intmask, r_uint, r_uint32, is_valid_int, r_ulonglong, r_longlong, r_int64

def empty_object():
    return instantiate(W_PointersObject)

def empty_symbol():
    return instantiate(W_BytesObject)

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
    def __init__(self):
        # This is a hack; see compile_code() in main.py
        self.suppress_process_switch = QuasiConstant(False)
        self.headless = QuasiConstant(False)
        self.highdpi = QuasiConstant(False)
        self.software_renderer = QuasiConstant(False)
        self.no_display = QuasiConstant(False)
        self.silent = QuasiConstant(False)
        self.omit_printing_raw_bytes = QuasiConstant(False)
        self.image_loaded = QuasiConstant(False)
        self.is_spur = QuasiConstant(False)
        self.uses_block_contexts = QuasiConstant(False)
        self.simulate_numeric_primitives = QuasiConstant(False)

        self.system_attributes = {}
        self._system_attribute_version = QuasiConstant(Version())
        self._executable_path = QuasiConstant("")
        self.title = QuasiConstant("RSqueak")
        self.altf4quit = QuasiConstant(False)

        from rsqueakvm.display import NullDisplay
        self._display = QuasiConstant(None, cls=NullDisplay)
        from rsqueakvm.interpreter import Interpreter
        self.interp = QuasiConstant(None, cls=Interpreter)

        self.make_special_objects()
        self.strategy_factory = storage.StrategyFactory(self)

    def make_special_objects(self):
        # These are used in the interpreter bytecodes
        self.w_minus_one = W_SmallInteger(-1)
        self.w_zero = W_SmallInteger(0)
        self.w_one = W_SmallInteger(1)
        self.w_two = W_SmallInteger(2)
        self.w_special_objects = empty_object()
        # no add all of those special objects that we assume constant while the image is running
        for name, (idx, t) in constants.constant_objects_in_special_object_table.items():
            if t == "POINTERS":
                setattr(self, "w_" + name, empty_object())
            elif t == "BYTES":
                setattr(self, "w_" + name, empty_symbol())
            else:
                assert False

    def runtime_setup(self, interp, exepath, argv, image_name, image_args_idx):
        self.interp.set(interp)
        fullpath = exepath
        self._executable_path.set(fullpath)
        for i in range(image_args_idx, len(argv)):
            self.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_ARGS_INDEX + i - image_args_idx, argv[i])
        self.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, image_name)
        self.image_loaded.activate()
        self.init_system_attributes(argv)

        from rsqueakvm.plugins import PluginRegistry
        [p.startup(self, argv) for p in PluginRegistry.enabled_plugins]

    def init_system_attributes(self, argv):
        for i in xrange(1, len(argv)):
            self.set_system_attribute(-i, argv[i])
        import platform
        from rsqueakvm.main import VERSION, BUILD_DATE
        from rpython.rlib.compilerinfo import get_compiler_info
        self.set_system_attribute(0, self._executable_path.get())
        self.set_system_attribute(1001, platform.system())    # operating system
        self.set_system_attribute(1002, platform.version())   # operating system version
        self.set_system_attribute(1003, platform.processor())  # platform's processor type
        self.set_system_attribute(1004, VERSION)
        self.set_system_attribute(1006, "%s Compiler: %s" % (BUILD_DATE, get_compiler_info()))
        self.set_system_attribute(1007, "rsqueak")            # interpreter class (invented for Cog)

    def get_system_attribute(self, idx):
        return self._pure_get_system_attribute(idx, self._system_attribute_version.get())

    @jit.elidable
    def _pure_get_system_attribute(self, idx, version):
        return self.system_attributes[idx]

    def set_system_attribute(self, idx, value):
        self.system_attributes[idx] = value
        self._system_attribute_version.changed()

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
                return self.wrap_wordint_direct(r_uint(-val), self.w_LargeNegativeInteger)
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

    def wrap_symbol(self, string):
        w_inst = self.wrap_string(string)
        w_inst.change_class(self, self.w_ByteSymbol)
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

    def get_image_name(self):
        return self.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX)

    def window_title(self):
        title = self.title.get()
        imgpath = self.get_image_name()
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
        w_sd = self.w_smalltalkdict
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
            elif w_globals.instsize() == 4:
                w_array = w_globals.fetch(self, 1)
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

def add_special_properties():
    for n, i in constants.variables_in_special_object_table.items():
        def fun(name, idx):
            def getter(s): return s.w_special_objects.fetch(s, idx)
            getter.func_name = "w_" + name
            def setter(s, w_obj): return s.w_special_objects.store(s, idx, w_obj)
            setter.func_name = "set_w_" + name
            setattr(ObjSpace, "w_" + name, getter)
            setattr(ObjSpace, "set_w_" + name, setter)
        fun(n, i)
add_special_properties()
