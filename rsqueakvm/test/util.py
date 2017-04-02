import py
import sys

from rsqueakvm import storage_classes, interpreter, objspace, util, constants, squeakimage, interpreter_bytecodes
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import EXTERNAL_CALL

from rpython.rlib import jit
from rpython.rlib.objectmodel import instantiate


IMAGENAME = "anImage.image"

def mock(space, stack, context = None):
    mapped_stack = [space.w(x) for x in stack]
    frame = context
    for i in range(len(stack)):
        frame.as_context_get_shadow(space).push(stack[i])
    interp = InterpreterForTest(space)
    interp.space.set_system_attribute(
        constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX,
        IMAGENAME)
    return interp, frame, len(stack)

def _prim(space, code, stack, context = None):
    interp, w_frame, argument_count = mock(space, stack, context)
    prim_table[code](interp,
                     w_frame.as_context_get_shadow(space),
                     argument_count-1,
                     context and context.as_context_get_shadow(space).w_method())
    res = w_frame.as_context_get_shadow(space).pop()
    s_frame = w_frame.as_context_get_shadow(space)
    assert not s_frame.stackdepth()-s_frame.tempsize() # check args are consumed
    return res

def external_call(space, module_name, method_name, stack):
    stack = [space.w(o) for o in stack]
    w_description = W_PointersObject(space, space.w_Array, 2)
    w_description.atput0(space, 0, space.w(module_name))
    w_description.atput0(space, 1, space.w(method_name))
    context = space.make_frame("<not called>",
                               [w_description],
                               stack[0], stack[1:])[0]
    return _prim(space, EXTERNAL_CALL, stack, context)


# Use these as decorators, if the test takes longer then a few seconds.
# The according options is configured in conftest.py.
# To mark all tests in a module as slow, add this line to the module:
# pytestmark = slow_test
slow_test = py.test.mark.skipif('not config.getvalue("execute-quick-tests")',
                        reason="Slow tests are being skipped because of -Q|--quick option.")

very_slow_test = py.test.mark.skipif('not config.getvalue("execute-slow-tests")',
                        reason="Very slow tests are being skipped. Add -S|--slow to execute all tests.")

# Skip unconditionally
def skip(reason=""):
    return py.test.mark.skipif('True', reason=reason)

# Most tests don't need a bootstrapped objspace. Those that do, indicate so explicitely.
# This way, as many tests as possible use the real, not-bootstrapped ObjSpace.
bootstrap_by_default = False

image_dir = py.path.local(__file__).dirpath('images')

def image_path(imagefilename):
    import os
    if os.path.isabs(imagefilename):
        return imagefilename
    else:
        return image_dir.join(imagefilename).strpath

def image_stream(imagefilename):
    return squeakimage.Stream(filename=image_path(imagefilename))

def open_reader(space, imagefilename):
    return squeakimage.ImageReader(space, image_stream(imagefilename))

image_cache = {}

def read_image(image_filename, space=None, cached=True):
    if cached and image_filename in image_cache:
        space, reader, image = image_cache.get(image_filename)
    else:
        if space is None:
            space = create_space()
        reader = open_reader(space, image_filename)
        image = reader.create_image()
        image_cache[image_filename] = (space, reader, image)
    interp = InterpreterForTest(space, image)
    return space, interp, image, reader

def create_space(bootstrap = bootstrap_by_default):
    space = BootstrappedObjSpace()
    space.setup()
    if bootstrap:
        space.bootstrap()
        space.uses_block_contexts.activate()
    return space

def create_space_interp(bootstrap = bootstrap_by_default):
    space = create_space(bootstrap)
    interp = InterpreterForTest(space)
    return space, interp

def copy_to_module(locals, module_name, all_tests_slow = False):
    mod = sys.modules[module_name]
    mod._copied_objects_ = []
    for name, obj in locals.items():
        setattr(mod, name, obj)
        mod._copied_objects_.append(name)

def cleanup_module(module_name):
    mod = sys.modules[module_name]
    if hasattr(mod, "_copied_objects_"):
        for name in mod._copied_objects_:
            delattr(mod, name)
        del mod._copied_objects_
        import gc; gc.collect()

def import_bytecodes(module_name):
    # expose the bytecode's values as global constants.
    # Bytecodes that have a whole range are exposed as global functions:
    # call them with an argument 'n' to get the bytecode number 'base + n'.
    mod = sys.modules[module_name]
    def make_getter(entry):
        def get_opcode_chr(n):
            opcode = entry[0] + n
            assert entry[0] <= opcode <= entry[1]
            return chr(opcode)
        setattr(mod, name, get_opcode_chr)
    for entry in interpreter_bytecodes.BYTECODE_RANGES:
        name = entry[-1]
        if len(entry) == 2:     # no range
            setattr(mod, name, chr(entry[0]))
        else:
            make_getter(entry)

class TestImage():
    def __init__(self, space):
        self.version = squeakimage.ImageVersion(0, False, False, False, False, False)

# This interpreter allows fine grained control of the interpretation
# by manually stepping through the bytecodes, if _loop is set to False.
class InterpreterForTest(interpreter.Interpreter):
    _loop = False

    def __init__(self, *args, **kwargs):
        interpreter.Interpreter.__init__(self, *args, **kwargs)
        if not self.image:
            self.image = TestImage(self.space)

    def loop(self, w_active_context):
        self._loop = True
        return interpreter.Interpreter.loop(self, w_active_context)

    def stack_frame(self, s_new_frame, s_sender, may_context_switch):
        if not self._loop:
            # this test is done to not loop in test, but rather step just once where wanted
            # Unfortunately, we have to mimick some of the original behaviour.
            s_new_frame.store_s_sender(s_sender)
            return s_new_frame
        return interpreter.Interpreter.stack_frame(self, s_new_frame, s_sender, may_context_switch)

    # ============ Helpers for executing ============

    def interpret_bc(self, bcodes, literals=None, receiver=None):
        w_frame, s_frame = self.space.make_frame(bcodes, literals=literals, receiver=receiver)
        self.space.wrap_frame(s_frame)
        return self.interpret_toplevel(w_frame)

    def execute_method(self, w_method):
        s_frame = w_method.create_frame(self.space, self.space.w(0))
        self.space.wrap_frame(s_frame)
        try:
            self.loop(s_frame.w_self())
        except interpreter.ReturnFromTopLevel, e:
            return e.object
        assert False, "Frame did not return correctly."

class BootstrappedObjSpace(objspace.ObjSpace):

    def __init__(self):
        objspace.ObjSpace.__init__(self)
        self.headless.activate()
        self.testing = True

    def setup(self):
        self.set_system_attribute(constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, "BootstrappedImage")
        self.image_loaded.activate()
        self.init_system_attributes([])

    def bootstrap(self):
        # Fill this ObjSpace up with class complete core hierarchies and patch core objects.
        self.create_core_classes()
        self.patch_bootstrap_classes()
        self.patch_bootstrap_objects()

    def create_core_classes(self):
        def define_core_cls(name, w_superclass, w_metaclass):
            assert name.startswith('w_')
            w_class = self.bootstrap_class(instsize=6,    # XXX
                                      w_superclass=w_superclass,
                                      w_metaclass=w_metaclass,
                                      name=name[2:])
            setattr(self, name, w_class)
            return w_class

        #   A complete minimal setup (including Behavior) would look like this
        #
        #   class:              superclass:         metaclass:
        #   ------------------- ------------------- -------------------
        #   Object              *nil                 Object class
        #   Behavior            Object              Behavior class
        #   ClassDescription    Behavior            ClassDescription class
        #   Class               ClassDescription    Class class
        #   Metaclass           ClassDescription    Metaclass class
        #   Object class        *Class              *Metaclass
        #   Behavior class      Object class        *Metaclass
        #   ClassDescription cl Behavior class      *Metaclass
        #   Class class         ClassDescription cl *Metaclass
        #   Metaclass class     ClassDescription cl *Metaclass

        #    Class Name            Super class name
        cls_nm_tbl = [
            ["w_Object",           "w_ProtoObject"], # there is not ProtoObject in mini.image
            ["w_Behavior",         "w_Object"],
            ["w_ClassDescription", "w_Behavior"],
            ["w_Class",            "w_ClassDescription"],
            ["w_Metaclass",        "w_ClassDescription"],
            ]
        define_core_cls("w_ProtoObjectClass", None, None)
        w_ProtoObjectClass = self.w_ProtoObjectClass
        define_core_cls("w_ProtoObject", None, w_ProtoObjectClass)
        for (cls_nm, super_cls_nm) in cls_nm_tbl:
            meta_nm = cls_nm + "Class"
            meta_super_nm = super_cls_nm + "Class"
            w_metacls = define_core_cls(meta_nm, getattr(self, meta_super_nm), None)
            define_core_cls(cls_nm, getattr(self, super_cls_nm), w_metacls)
        proto_shadow = w_ProtoObjectClass.strategy
        proto_shadow.store_w_superclass(self.w_Class)
        # at this point, all classes that still lack a w_class are themselves metaclasses
        for nm, idx in constants.constant_objects_in_special_object_table_wo_types.items():
            w_cls_obj = getattr(self, "w_" + nm)
            if nm[0].isupper():
                if w_cls_obj.getclass(None) is None:
                    if w_cls_obj.strategy is None:
                        w_cls_obj._initialize_storage(self, self.w_Metaclass, 0)
                    elif w_cls_obj.strategy.w_class is None:
                        w_cls_obj.strategy.w_class = self.w_Metaclass
                    else:
                        import pdb; pdb.set_trace()

    def patch_bootstrap_classes(self):
        # Create all classes in the class hierarchies of the classes in the special objects array.
        def create_metaclass(cls_nm, supercls_nm):
            meta_nm = cls_nm + "Class"
            meta_super_nm = supercls_nm + "Class"
            w_meta_cls = self.bootstrap_class(0,   # XXX
                                         getattr(self, meta_super_nm),
                                         self.w_Metaclass,
                                         name=meta_nm[2:])
            setattr(self, meta_nm, w_meta_cls)
            return w_meta_cls
        def define_cls(cls_nm, supercls_nm, instvarsize=0, format=storage_classes.POINTERS, varsized=False):
            assert cls_nm.startswith("w_")
            w_meta_cls = create_metaclass(cls_nm, supercls_nm)
            w_cls = self.bootstrap_class(instvarsize,
                                         getattr(self, supercls_nm),
                                         w_meta_cls,
                                         format=format,
                                         varsized=varsized,
                                         name=cls_nm[2:])
            setattr(self, cls_nm, w_cls)
            return w_cls
        define_cls("w_Magnitude", "w_Object")
        define_cls("w_Number", "w_Magnitude")
        define_cls("w_Integer", "w_Number")
        define_cls("w_Collection", "w_Object")
        define_cls("w_SequenceableCollection", "w_Collection")
        define_cls("w_ArrayedCollection", "w_SequenceableCollection")
        define_cls("w_MethodDict", "w_Object", instvarsize=2, varsized=True)
        define_cls("w_ContextPart", "w_Object")
        define_cls("w_Link", "w_Object")
        define_cls("w_LinkedList", "w_SequenceableCollection")
        define_cls("w_LookupKey", "w_Magnitude")
        define_cls("w_Binding", "w_LookupKey")
        define_cls("w_ClassBinding", "w_Binding")

        # Also create classes for the objects in the special objects array
        define_cls("w_UndefinedObject", "w_Object")
        define_cls("w_Boolean", "w_Object")
        define_cls("w_True", "w_Boolean")
        define_cls("w_False", "w_Boolean")

        # Now patch up the already created special classes
        def patch_special_cls(cls_nm, supercls_nm, instvarsize=0, format=storage_classes.POINTERS, varsized=False):
            assert cls_nm.startswith("w_")
            w_meta_cls = create_metaclass(cls_nm, supercls_nm)

            # Now patch up the existing class object
            w_cls = getattr(self, cls_nm)
            assert w_cls, "This class should have been created in ObjSpace!"
            self.patch_class(w_cls,
                        instvarsize,
                        getattr(self, supercls_nm),
                        w_meta_cls,
                        format=format,
                        varsized=varsized,
                        name=cls_nm[2:])
        patch_special_cls("w_Bitmap", "w_ArrayedCollection", varsized=True, format=storage_classes.WORDS)
        patch_special_cls("w_SmallInteger", "w_Integer")
        patch_special_cls("w_String", "w_ArrayedCollection", format=storage_classes.BYTES)
        patch_special_cls("w_Array", "w_ArrayedCollection", varsized=True)
        patch_special_cls("w_Float", "w_Number", format=storage_classes.BYTES)
        patch_special_cls("w_MethodContext", "w_ContextPart")
        patch_special_cls("w_BlockContext", "w_ContextPart", instvarsize=constants.BLKCTX_STACK_START)
        patch_special_cls("w_BlockClosure", "w_Object", instvarsize=constants.BLKCLSR_SIZE, varsized=True)
        patch_special_cls("w_Point", "w_Object", instvarsize=2)
        patch_special_cls("w_LargePositiveInteger", "w_Integer", format=storage_classes.BYTES)
        patch_special_cls("w_LargeNegativeInteger", "w_LargePositiveInteger", format=storage_classes.BYTES)
        patch_special_cls("w_Message", "w_Object")
        patch_special_cls("w_ByteArray", "w_ArrayedCollection", format=storage_classes.BYTES)
        patch_special_cls("w_CompiledMethod", "w_ByteArray", format=storage_classes.COMPILED_METHOD)
        patch_special_cls("w_Semaphore", "w_LinkedList")
        patch_special_cls("w_Character", "w_Magnitude", instvarsize=1)
        patch_special_cls("w_Process", "w_Link")

    def patch_bootstrap_objects(self):
        def patch_bootstrap_object(obj, cls, size):
            obj._initialize_storage(self, cls, size)
        patch_bootstrap_object(self.w_nil, self.w_UndefinedObject, 0)
        patch_bootstrap_object(self.w_true, self.w_True, 0)
        patch_bootstrap_object(self.w_false, self.w_False, 0)
        patch_bootstrap_object(self.w_special_selectors, self.w_Array, len(constants.SPECIAL_SELECTORS) * 2)
        special_objects_w = [self.w_nil] * (constants.SPECIAL_OBJECTS_SIZE * 2)
        for name, idx in constants.constant_objects_in_special_object_table_wo_types.items():
            special_objects_w[idx] = getattr(self, "w_" + name)
        self.w_special_objects = self.wrap_list(special_objects_w)
        self.w_schedulerassociationpointer = (
            self.wrap_list([ # assoc
                self.w_nil,
                self.wrap_list([ # scheduler
                    self.w_nil,
                    self.wrap_list([self.w_nil, self.w_nil]) # active proc
                ])
            ])
        )

    def patch_class(self, w_class, instsize, w_superclass=None, w_metaclass=None,
                        name='?', format=storage_classes.POINTERS, varsized=False):
        s = instantiate(storage_classes.ClassShadow)
        s.space = self
        s.w_class = w_metaclass
        s.version = util.version.Version()
        s._w_self = w_class
        s.subclass_s = {}
        s._s_superclass = None
        s.store_w_superclass(w_superclass)
        s.name = name
        s._instance_size = instsize
        s.instance_kind = format
        s._s_methoddict = None
        s.instance_varsized = varsized or format != storage_classes.POINTERS
        w_class.store_strategy(s)
        s._initialize_storage(w_class, 6)

    def bootstrap_class(self, instsize, w_superclass=None, w_metaclass=None,
                        name='?', format=storage_classes.POINTERS, varsized=False):
        w_class = W_PointersObject(self, w_metaclass, 6)
        self.patch_class(w_class, instsize, w_superclass, w_metaclass, name, format, varsized)
        return w_class

    def w(self, any):
        from rpython.rlib.rarithmetic import r_int64
        from rpython.rlib.rbigint import rbigint

        def looooong(val):
            return rbigint.fromlong(val)
        if False: pass
        elif any is None: return self.w_nil
        elif isinstance(any, W_Object): return any
        elif isinstance(any, long): return self.wrap_int(looooong(any))
        elif isinstance(any, bool): return self.wrap_bool(any)
        elif isinstance(any, int): return self.wrap_int(any)
        elif isinstance(any, float): return self.wrap_float(any)
        elif isinstance(any, list): return self.wrap_list(any)
        elif isinstance(any, str):
            # assume never have strings of length 1
            if len(any) == 1:
                return self.wrap_char(any)
            else:
                return self.wrap_string(any)
        else:
            raise Exception("Cannot wrap %r" % any)

    def initialize_class(self, w_class, interp):
        initialize_symbol = self.find_symbol_in_methoddict("initialize",
                            w_class.class_shadow(self))
        interp.perform(w_class, w_selector=initialize_symbol)

    def find_symbol_in_methoddict(self, string, cls, fail=True):
        if isinstance(cls, W_PointersObject):
            cls = cls.as_class_get_shadow(self)
        s_methoddict = cls.s_methoddict()
        s_methoddict.sync_method_cache()
        methoddict_w = s_methoddict.methoddict
        for each in methoddict_w.keys():
            if each.unwrap_string(None) == string:
                return each
        if fail:
            assert False, 'Using image without %s method in class %s.' % (string, cls.name)
        else:
            return None

    # ============ Helpers for executing ============

    def wrap_frame(self, s_frame):
        # Add a toplevel frame around s_frame to properly return.
        toplevel_frame = self.make_method([0x7c]).create_frame(self, self.w(0), [])
        s_frame.store_s_sender(toplevel_frame)

    def make_method(self, bytes, literals=None, numargs=0):
        if not isinstance(bytes, str):
            bytes = "".join([chr(x) for x in bytes])
        w_method = W_PreSpurCompiledMethod(self, len(bytes))
        w_method.islarge = 1
        w_method.bytes = bytes
        w_method.argsize=numargs
        w_method._tempsize=8
        if literals is None:
            literals = [W_PointersObject(self, None, 2)]
        w_method.setliterals(literals)
        return w_method

    def make_frame(self, bytes, literals=None, receiver=None, args=[]):
        w_method = self.make_method(bytes, literals, len(args))
        if receiver is None:
            receiver = self.w_nil
        s_frame = w_method.create_frame(self, receiver, args)
        return s_frame.w_self(), s_frame
