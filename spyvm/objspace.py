import os

from spyvm import constants, model, wrapper, display, storage
from spyvm.error import UnwrappingError, WrappingError
from rpython.rlib import jit, rpath
from rpython.rlib.objectmodel import instantiate, specialize, import_from_mixin
from rpython.rlib.rarithmetic import intmask, r_uint, int_between, r_longlong, r_ulonglong, is_valid_int

class ConstantMixin(object):
    """Mixin for constant values that can be edited, but will be promoted
    to a constant when jitting."""

    def __init__(self, initial_value = None):
        if initial_value is None:
            initial_value = self.default_value
        self.value = [initial_value]

    def set(self, value):
        self.value[0] = value

    def get(self):
        value = jit.promote(self.value[0])
        return value

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
    def get(self):
        # Promoting does not work on strings...
        return self.value[0]

class ConstantObject(object):
    import_from_mixin(ConstantMixin)
    default_value = None

def empty_object():
    return instantiate(model.W_PointersObject)

class ObjSpace(object):
    _immutable_fields_ = ['objtable']

    def __init__(self):
        # This is a hack; see compile_code() in targetrsqueak.py
        self.suppress_process_switch = ConstantFlag()
        self.run_spy_hacks = ConstantFlag()
        self.headless = ConstantFlag()
        self.use_plugins = ConstantFlag()
        self.omit_printing_raw_bytes = ConstantFlag()
        self.image_loaded = ConstantFlag()

        self.classtable = {}
        self.objtable = {}
        self._executable_path = ConstantString()
        self._image_name = ConstantString()
        self._display = ConstantObject()

        # Create the nil object.
        # Circumvent the constructor because nil is already referenced there.
        w_nil = empty_object()
        w_nil.w_class = None
        self.add_bootstrap_object("w_nil", w_nil)

        self.strategy_factory = storage.StrategyFactory(self)
        self.make_bootstrap_classes()
        self.make_bootstrap_objects()

    def find_executable(self, executable):
        if os.sep in executable or (os.name == "nt" and ":" in executable):
            return executable
        path = os.environ.get("PATH")
        if path:
            for dir in path.split(os.pathsep):
                f = os.path.join(dir, executable)
                if os.path.isfile(f):
                    executable = f
                    break
        return rpath.rabspath(executable)

    def runtime_setup(self, executable, image_name):
        fullpath = rpath.rabspath(self.find_executable(executable))
        i = fullpath.rfind(os.path.sep) + 1
        assert i > 0
        self._executable_path.set(fullpath[:i])
        self._image_name.set(image_name)
        self.image_loaded.activate()

    def populate_special_objects(self, specials):
        for name, idx in constants.objects_in_special_object_table.items():
            name = "w_" + name
            if not name in self.objtable or not self.objtable[name]:
                try:
                    self.objtable[name] = specials[idx]
                except IndexError:
                    # if it's not yet in the table, the interpreter has to fill the gap later in populate_remaining_special_objects
                    self.objtable[name] = None
        # XXX this is kind of hacky, but I don't know where else to get Metaclass
        self.classtable["w_Metaclass"] = self.w_SmallInteger.w_class.w_class

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
        self.make_bootstrap_object("w_charactertable")
        self.make_bootstrap_object("w_true")
        self.make_bootstrap_object("w_false")
        self.make_bootstrap_object("w_special_selectors")
        self.add_bootstrap_object("w_minus_one", model.W_SmallInteger(-1))
        self.add_bootstrap_object("w_zero", model.W_SmallInteger(0))
        self.add_bootstrap_object("w_one", model.W_SmallInteger(1))
        self.add_bootstrap_object("w_two", model.W_SmallInteger(2))

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
        if isinstance(val, r_longlong) and not is_valid_int(val):
            if val > 0 and r_ulonglong(val) < r_uint(constants.U_MAXINT):
                return self.wrap_positive_32bit_int(intmask(val))
            else:
                raise WrappingError
        elif isinstance(val, r_uint):
            return self.wrap_positive_32bit_int(intmask(val))
        elif not is_valid_int(val):
            raise WrappingError
        # we don't do tagging
        return model.W_SmallInteger(intmask(val))

    def wrap_uint(self, val):
        from rpython.rlib.objectmodel import we_are_translated
        if val < 0:
            raise WrappingError("negative integer")
        else:
            return self.wrap_positive_32bit_int(intmask(val))

    def wrap_positive_32bit_int(self, val):
        # This will always return a positive value.
        # XXX: For now, we assume that val is at most 32bit, i.e. overflows are
        # checked for before wrapping. Also, we ignore tagging.
        if int_between(0, val, constants.MAXINT):
            return model.W_SmallInteger(val)
        else:
            return model.W_LargePositiveInteger1Word(val)

    def wrap_float(self, i):
        return model.W_Float(i)

    def wrap_string(self, string):
        w_inst = self.w_String.as_class_get_shadow(self).new(len(string))
        for i in range(len(string)):
            w_inst.setchar(i, string[i])
        return w_inst

    def wrap_char(self, c):
        return self.w_charactertable.fetch(self, ord(c))

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

    def unwrap_positive_32bit_int(self, w_value):
        return w_value.unwrap_positive_32bit_int(self)

    def unwrap_longlong(self, w_value):
        return w_value.unwrap_longlong(self)

    def unwrap_char(self, w_char):
        return w_char.unwrap_char(self)

    def unwrap_float(self, w_v):
        return w_v.unwrap_float(self)

    def unwrap_array(self, w_array):
        return w_array.unwrap_array(self)

    # ============= Access to static information =============

    @specialize.arg(1)
    def get_special_selector(self, selector):
        i0 = constants.find_selectorindex(selector)
        self.w_special_selectors.as_cached_object_get_shadow(self)
        return self.w_special_selectors.fetch(self, i0)

    def executable_path(self):
        return self._executable_path.get()

    def image_name(self):
        return self._image_name.get()

    def display(self):
        disp = self._display.get()
        if disp is None:
            # Create lazy to allow headless execution.
            disp = display.SDLDisplay(self.image_name())
            self._display.set(disp)
        return disp

    # ============= Other Methods =============

    def _freeze_(self):
        return True

    @jit.unroll_safe
    def newClosure(self, w_outer_ctxt, pc, numArgs, copiedValues):
        assert isinstance(w_outer_ctxt, model.W_PointersObject)
        pc_with_bytecodeoffset = pc + w_outer_ctxt.as_context_get_shadow(self).w_method().bytecodeoffset() + 1
        BlockClosureShadow = self.w_BlockClosure.as_class_get_shadow(self)
        numCopied = len(copiedValues)
        w_closure = BlockClosureShadow.new(numCopied)
        closure = wrapper.BlockClosureWrapper(self, w_closure)
        closure.store_outerContext(w_outer_ctxt)
        closure.store_startpc(pc_with_bytecodeoffset)
        closure.store_numArgs(numArgs)
        for i0 in range(numCopied):
            closure.atput0(i0, copiedValues[i0])
        return w_closure
