import os

from spyvm import constants, model, shadow, wrapper, version
from spyvm.error import UnwrappingError, WrappingError, PrimitiveFailedError
from rpython.rlib import jit, rpath
from rpython.rlib.objectmodel import instantiate, specialize
from rpython.rlib.rarithmetic import intmask, r_uint, int_between

class ObjSpace(object):
    def __init__(self):
        # If this is True, then no optimizing storage strategies will be used.
        # Intended for performance comparisons. Breaks tests.
        self.no_specialized_storage = [False]
        
        self.classtable = {}
        self.objtable = {}
        self._executable_path = [""] # XXX: we cannot set the attribute
                                  # directly on the frozen objectspace
        
        # Create the nil object.
        # Circumvent the constructor because nil is already referenced there.
        w_nil = instantiate(model.W_PointersObject)
        w_nil.w_class = None
        self.add_bootstrap_object("w_nil", w_nil)
        
        self.make_bootstrap_classes()
        self.make_bootstrap_objects()
        
        # This is a hack; see compile_code() in targetimageloadingsmalltalk.py
        self.suppress_process_switch = [False]

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

    def runtime_setup(self, executable):
        fullpath = rpath.rabspath(self.find_executable(executable))
        i = fullpath.rfind(os.path.sep) + 1
        assert i > 0
        self._executable_path[0] = fullpath[:i]

    def populate_special_objects(self, specials):
        for name, idx in constants.objects_in_special_object_table.items():
            name = "w_" + name
            if not name in self.objtable or not self.objtable[name]:
                self.objtable[name] = specials[idx]
        # XXX this is kind of hacky, but I don't know where else to get Metaclass
        self.classtable["w_Metaclass"] = self.w_SmallInteger.w_class.w_class
        
    def executable_path(self):
        return self._executable_path[0]
    
    def add_bootstrap_class(self, name, cls):
        self.classtable[name] = cls
        setattr(self, name, cls)
    
    def make_bootstrap_classes(self):
        names = [ "w_" + name for name in constants.classes_in_special_object_table.keys() ]
        for name in names:
            cls = model.W_PointersObject(self, None, 0)
            self.add_bootstrap_class(name, cls)
        
    def add_bootstrap_object(self, name, obj):
        self.objtable[name] = obj
        setattr(self, name, obj)
    
    def make_bootstrap_object(self, name):
        obj = model.W_PointersObject(self, None, 0)
        self.add_bootstrap_object(name, obj)
    
    def make_character_table(self):
        def build_char(i):
            # TODO - This is pretty hacky, maybe not required? At least eliminate the constant 1.
            w_cinst = model.W_PointersObject(self, self.w_Character, 1)
            w_cinst.store(self, constants.CHARACTER_VALUE_INDEX,
                          model.W_SmallInteger(i))
            return w_cinst
        char_table = model.W_PointersObject(self, self.classtable['w_Array'], 256)
        for i in range(256):
            char_table.store(self, i, build_char(i))
        self.add_bootstrap_object("w_charactertable", char_table)
    
    def make_bootstrap_objects(self):
        self.make_character_table()
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
    
    @specialize.arg(1)
    def get_special_selector(self, selector):
        i0 = constants.find_selectorindex(selector)
        self.w_special_selectors.as_cached_object_get_shadow(self)
        return self.w_special_selectors.fetch(self, i0)

    # methods for wrapping and unwrapping stuff

    def wrap_int(self, val):
        from spyvm import constants
        assert isinstance(val, int)
        # we don't do tagging
        return model.W_SmallInteger(val)

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

    def unwrap_int(self, w_value):
        if isinstance(w_value, model.W_SmallInteger):
            return intmask(w_value.value)
        elif isinstance(w_value, model.W_LargePositiveInteger1Word):
            if w_value.value >= 0:
                return intmask(w_value.value)
            else:
                raise UnwrappingError("The value is negative when interpreted as 32bit value.")
        raise UnwrappingError("expected a W_SmallInteger or W_LargePositiveInteger1Word, got %s" % (w_value,))

    def unwrap_uint(self, w_value):
        return w_value.unwrap_uint(self)

    def unwrap_positive_32bit_int(self, w_value):
        if isinstance(w_value, model.W_SmallInteger):
            if w_value.value >= 0:
                return r_uint(w_value.value)
        elif isinstance(w_value, model.W_LargePositiveInteger1Word):
            return r_uint(w_value.value)
        raise UnwrappingError("Wrong types or negative SmallInteger.")

    def unwrap_char(self, w_char):
        from spyvm import constants
        w_class = w_char.getclass(self)
        if not w_class.is_same_object(self.w_Character):
            raise UnwrappingError("expected character, got %s" % (w_class, ))
        w_ord = w_char.fetch(self, constants.CHARACTER_VALUE_INDEX)
        w_class = w_ord.getclass(self)
        if not w_class.is_same_object(self.w_SmallInteger):
            raise UnwrappingError("expected smallint from character, got %s" % (w_class, ))

        assert isinstance(w_ord, model.W_SmallInteger)
        return chr(w_ord.value)

    def unwrap_float(self, w_v):
        from spyvm import model
        if isinstance(w_v, model.W_Float): return w_v.value
        elif isinstance(w_v, model.W_SmallInteger): return float(w_v.value)
        raise UnwrappingError()

    @jit.look_inside_iff(lambda self, w_array: jit.isconstant(w_array.size()))
    def unwrap_array(self, w_array):
        # Check that our argument has pointers format and the class:
        if not w_array.getclass(self).is_same_object(self.w_Array):
            raise UnwrappingError()
        assert isinstance(w_array, model.W_PointersObject)

        return [w_array.at0(self, i) for i in range(w_array.size())]

    def get_display(self):
        w_display = self.objtable['w_display']
        if w_display:
            w_bitmap = w_display.fetch(self, 0)
            if isinstance(w_bitmap, model.W_DisplayBitmap):
                return w_bitmap.display
        raise PrimitiveFailedError("No display")

    def _freeze_(self):
        return True

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
