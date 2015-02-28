import py, sys
from spyvm import model, storage_classes, objspace, util, constants, squeakimage, interpreter, interpreter_bytecodes
from rpython.rlib.objectmodel import instantiate

# Use these as decorators, if the test takes longer then a few seconds.
# The according options is configured in conftest.py.
# To mark all tests in a module as slow, add this line to the module:
# pytestmark = slow_test
slow_test = py.test.mark.skipif('not config.getvalue("execute-quick-tests")',
                        reason="Slow tests are being skipped because of -Q|--quick option.")

very_slow_test = py.test.mark.skipif('not config.getvalue("execute-slow-tests")',
                        reason="Very slow tests are being skipped. Add -S|--slow to execute all tests.")

# Most tests don't need a bootstrapped objspace. Those that do, indicate so explicitely.
# This way, as many tests as possible use the real, not-bootstrapped ObjSpace.
bootstrap_by_default = False

image_dir = py.path.local(__file__).dirpath('images')

def image_path(imagefilename):
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
    interp = TestInterpreter(space, image)
    interp.populate_remaining_special_objects()
    return space, interp, image, reader

def create_space(bootstrap = bootstrap_by_default):
    space = BootstrappedObjSpace()
    if bootstrap:
        space.bootstrap()
    return space

def create_space_interp(bootstrap = bootstrap_by_default):
    space = create_space(bootstrap)
    interp = TestInterpreter(space)
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

# This interpreter allows fine grained control of the interpretation
# by manually stepping through the bytecodes, if _loop is set to False.
class TestInterpreter(interpreter.Interpreter):
    _loop = False
    
    def loop(self, w_active_context):
        self._loop = True
        return interpreter.Interpreter.loop(self, w_active_context)
    
    def stack_frame(self, s_new_frame, s_sender, may_context_switch=True):
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
    
    def bootstrap(self):
        # Fill this ObjSpace up with class complete core hierarchies and patch core objects.
        self.create_core_classes()
        self.patch_bootstrap_classes()
        self.patch_bootstrap_objects()
    
    def create_core_classes(self):
        def define_core_cls(name, w_superclass, w_metaclass):
            assert name.startswith('w_')
            w_class = self.bootstrap_class(instsize=0,    # XXX
                                      w_superclass=w_superclass,
                                      w_metaclass=w_metaclass,
                                      name=name[2:])
            self.add_bootstrap_class(name, w_class)
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
        w_ProtoObjectClass = self.classtable["w_ProtoObjectClass"]
        define_core_cls("w_ProtoObject", None, w_ProtoObjectClass)
        for (cls_nm, super_cls_nm) in cls_nm_tbl:
            meta_nm = cls_nm + "Class"
            meta_super_nm = super_cls_nm + "Class"
            w_metacls = define_core_cls(meta_nm, self.classtable[meta_super_nm], None)
            define_core_cls(cls_nm, self.classtable[super_cls_nm], w_metacls)
        proto_shadow = w_ProtoObjectClass.strategy
        proto_shadow.store_w_superclass(self.w_Class)
        # at this point, all classes that still lack a w_class are themselves metaclasses
        for nm, w_cls_obj in self.classtable.items():
            if w_cls_obj.w_class is None:
                w_cls_obj.w_class = self.w_Metaclass
    
    def patch_bootstrap_classes(self):
        # Create all classes in the class hierarchies of the classes in the special objects array.
        def create_metaclass(cls_nm, supercls_nm):
            meta_nm = cls_nm + "Class"
            meta_super_nm = supercls_nm + "Class"
            w_meta_cls = self.bootstrap_class(0,   # XXX
                                         self.classtable[meta_super_nm],
                                         self.w_Metaclass,
                                         name=meta_nm[2:])
            self.add_bootstrap_class(meta_nm, w_meta_cls)
            return w_meta_cls
        def define_cls(cls_nm, supercls_nm, instvarsize=0, format=storage_classes.POINTERS, varsized=False):
            assert cls_nm.startswith("w_")
            w_meta_cls = create_metaclass(cls_nm, supercls_nm)
            w_cls = self.bootstrap_class(instvarsize,
                                         self.classtable[supercls_nm],
                                         w_meta_cls,
                                         format=format,
                                         varsized=varsized,
                                         name=cls_nm[2:])
            self.add_bootstrap_class(cls_nm, w_cls)
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
            w_cls = self.classtable[cls_nm]
            assert w_cls, "This class should have been created in ObjSpace!"
            self.patch_class(w_cls,
                        instvarsize,
                        self.classtable[supercls_nm],
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
        patch_special_cls("w_Point", "w_Object")
        patch_special_cls("w_LargePositiveInteger", "w_Integer", format=storage_classes.BYTES)
        patch_special_cls("w_Message", "w_Object")
        patch_special_cls("w_ByteArray", "w_ArrayedCollection", format=storage_classes.BYTES)
        patch_special_cls("w_CompiledMethod", "w_ByteArray", format=storage_classes.COMPILED_METHOD)
        patch_special_cls("w_Semaphore", "w_LinkedList")
        patch_special_cls("w_Character", "w_Magnitude", instvarsize=1)
        patch_special_cls("w_Process", "w_Link")
        
    def patch_bootstrap_objects(self):
        def patch_bootstrap_object(obj, cls, size):
            obj.w_class = cls
            obj.initialize_storage(self, size)
        patch_bootstrap_object(self.w_nil, self.w_UndefinedObject, 0)
        patch_bootstrap_object(self.w_true, self.w_True, 0)
        patch_bootstrap_object(self.w_false, self.w_False, 0)
        patch_bootstrap_object(self.w_special_selectors, self.w_Array, len(constants.SPECIAL_SELECTORS) * 2)
        patch_bootstrap_object(self.w_charactertable, self.w_Array, 256)
        
        # Bootstrap character table
        for i in range(256):
            w_cinst = model.W_PointersObject(self, self.w_Character, 1)
            w_cinst.store(self, constants.CHARACTER_VALUE_INDEX, model.W_SmallInteger(i))
            self.w_charactertable.store(self, i, w_cinst)
    
    def patch_class(self, w_class, instsize, w_superclass=None, w_metaclass=None,
                        name='?', format=storage_classes.POINTERS, varsized=False):
        s = instantiate(storage_classes.ClassShadow)
        s.space = self
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
        s.initialize_storage(w_class, 0)
        w_class.w_class = w_metaclass
        
    def bootstrap_class(self, instsize, w_superclass=None, w_metaclass=None,
                        name='?', format=storage_classes.POINTERS, varsized=False):
        w_class = model.W_PointersObject(self, w_metaclass, 0)
        self.patch_class(w_class, instsize, w_superclass, w_metaclass, name, format, varsized)
        return w_class

    def w(self, any):
        if any is None: return self.w_nil
        if isinstance(any, model.W_Object): return any
        if isinstance(any, str):
            # assume never have strings of length 1
            if len(any) == 1:
                return self.wrap_char(any)
            else:
                return self.wrap_string(any)
        if isinstance(any, long): return self.wrap_long(any)
        if isinstance(any, bool): return self.wrap_bool(any)
        if isinstance(any, int): return self.wrap_int(any)
        if isinstance(any, float): return self.wrap_float(any)
        if isinstance(any, list): return self.wrap_list(any)
        raise Exception("Cannot wrap %r" % any)
    
    def wrap_long(self, any):
        assert any >= 0
        import struct
        bytes = struct.pack('L', any)
        w_b = model.W_BytesObject(self, self.w_LargePositiveInteger, len(bytes))
        w_b.bytes = [c for c in bytes]
        return w_b

    def initialize_class(self, w_class, interp):
        initialize_symbol = self.find_symbol_in_methoddict("initialize", 
                            w_class.class_shadow(self))
        interp.perform(w_class, w_selector=initialize_symbol)
    
    def find_symbol_in_methoddict(self, string, cls):
        if isinstance(cls, model.W_PointersObject):
            cls = cls.as_class_get_shadow(self)
        s_methoddict = cls.s_methoddict()
        s_methoddict.sync_method_cache()
        methoddict_w = s_methoddict.methoddict
        for each in methoddict_w.keys():
            if each.as_string() == string:
                return each
        assert False, 'Using image without %s method in class %s.' % (string, cls.name)

    # ============ Helpers for executing ============
    
    def wrap_frame(self, s_frame):
        # Add a toplevel frame around s_frame to properly return.
        toplevel_frame = self.make_method([0x7c]).create_frame(self, self.w(0), [])
        s_frame.store_s_sender(toplevel_frame)
        
    def make_method(self, bytes, literals=None, numargs=0):
        if not isinstance(bytes, str):
            bytes = "".join([chr(x) for x in bytes])
        w_method = model.W_CompiledMethod(self, len(bytes))
        w_method.islarge = 1
        w_method.bytes = bytes
        w_method.argsize=numargs
        w_method._tempsize=8
        if literals is None:
            literals = [model.W_PointersObject(self, None, 2)]
        w_method.setliterals(literals)
        return w_method
    
    def make_frame(self, bytes, literals=None, receiver=None, args=[]):
        w_method = self.make_method(bytes, literals, len(args))
        if receiver is None:
            receiver = self.w_nil
        s_frame = w_method.create_frame(self, receiver, args)
        return s_frame.w_self(), s_frame
        
