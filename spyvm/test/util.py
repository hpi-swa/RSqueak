from spyvm import model, shadow, objspace, version, constants
from rpython.rlib.objectmodel import instantiate

class BootstrappedObjSpace(objspace.ObjSpace):
    
    def make_bootstrap_classes(self):
        def define_core_cls(name, w_superclass, w_metaclass):
            assert name.startswith('w_')
            w_class = bootstrap_class(self, instsize=0,    # XXX
                                      w_superclass=w_superclass,
                                      w_metaclass=w_metaclass,
                                      name=name[2:])
            self.classtable[name] = w_class
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
        w_Class = self.classtable["w_Class"]
        w_Metaclass = self.classtable["w_Metaclass"]
        # XXX
        proto_shadow = w_ProtoObjectClass.shadow
        proto_shadow.store_w_superclass(w_Class)
        # at this point, all classes that still lack a w_class are themselves
        # metaclasses
        for nm, w_cls_obj in self.classtable.items():
            if w_cls_obj.w_class is None:
                w_cls_obj.w_class = w_Metaclass

        def define_cls(cls_nm, supercls_nm, instvarsize=0, format=shadow.POINTERS,
                       varsized=False):
            assert cls_nm.startswith("w_")
            meta_nm = cls_nm + "Class"
            meta_super_nm = supercls_nm + "Class"
            w_Metaclass = self.classtable["w_Metaclass"]
            w_meta_cls = self.classtable[meta_nm] = \
                         bootstrap_class(self, 0,   # XXX
                                         self.classtable[meta_super_nm],
                                         w_Metaclass,
                                         name=meta_nm[2:])
            w_cls = self.classtable[cls_nm] = \
                         bootstrap_class(self, instvarsize,
                                         self.classtable[supercls_nm],
                                         w_meta_cls,
                                         format=format,
                                         varsized=varsized,
                                         name=cls_nm[2:])

        define_cls("w_Magnitude", "w_Object")
        define_cls("w_Character", "w_Magnitude", instvarsize=1)
        define_cls("w_Number", "w_Magnitude")
        define_cls("w_Integer", "w_Number")
        define_cls("w_SmallInteger", "w_Integer")
        define_cls("w_LargePositiveInteger", "w_Integer", format=shadow.BYTES)
        define_cls("w_Float", "w_Number", format=shadow.BYTES)
        define_cls("w_Message", "w_Object")
        define_cls("w_Collection", "w_Object")
        define_cls("w_SequenceableCollection", "w_Collection")
        define_cls("w_ArrayedCollection", "w_SequenceableCollection")
        define_cls("w_Array", "w_ArrayedCollection", varsized=True)
        define_cls("w_String", "w_ArrayedCollection", format=shadow.BYTES)
        define_cls("w_Bitmap", "w_ArrayedCollection", varsized=True, format=shadow.WORDS)
        define_cls("w_UndefinedObject", "w_Object")
        define_cls("w_Boolean", "w_Object")
        define_cls("w_True", "w_Boolean")
        define_cls("w_False", "w_Boolean")
        define_cls("w_ByteArray", "w_ArrayedCollection", format=shadow.BYTES)
        define_cls("w_MethodDict", "w_Object", instvarsize=2, varsized=True)
        define_cls("w_CompiledMethod", "w_ByteArray", format=shadow.COMPILED_METHOD)
        define_cls("w_ContextPart", "w_Object")
        define_cls("w_MethodContext", "w_ContextPart")
        define_cls("w_Link", "w_Object")
        define_cls("w_Process", "w_Link")
        define_cls("w_Point", "w_Object")
        define_cls("w_LinkedList", "w_SequenceableCollection")
        define_cls("w_Semaphore", "w_LinkedList")
        define_cls("w_BlockContext", "w_ContextPart",
                   instvarsize=constants.BLKCTX_STACK_START)
        define_cls("w_BlockClosure", "w_Object",
                   instvarsize=constants.BLKCLSR_SIZE,
                   varsized=True)
        # make better accessors for classes that can be found in special object
        # table
        for name in constants.classes_in_special_object_table.keys():
            name = 'w_' + name
            setattr(self, name, self.classtable.get(name))

    def make_bootstrap_objects(self):
        def bld_char(i):
            w_cinst = self.w_Character.as_class_get_shadow(self).new()
            w_cinst.store(self, constants.CHARACTER_VALUE_INDEX,
                          model.W_SmallInteger(i))
            return w_cinst
        w_charactertable = model.W_PointersObject(self,
            self.classtable['w_Array'], 256)
        self.w_charactertable = w_charactertable
        for i in range(256):
            self.w_charactertable.atput0(self, i, bld_char(i))


        # Very special nil hack: in order to allow W_PointersObject's to
        # initialize their fields to nil, we have to create it in the model
        # package, and then patch up its fields here:
        def patch_nil(w_nil):
            w_nil.space = self
            w_nil.s_class = self.classtable['w_UndefinedObject'].as_class_get_shadow(self)
            w_nil.initialize_storage(self, 0)
            return w_nil
        w_nil = self.w_nil = patch_nil(model.w_nil)

        w_true = self.classtable['w_True'].as_class_get_shadow(self).new()
        self.w_true = w_true
        w_false = self.classtable['w_False'].as_class_get_shadow(self).new()
        self.w_false = w_false
        self.w_minus_one = model.W_SmallInteger(-1)
        self.w_zero = model.W_SmallInteger(0)
        self.w_one = model.W_SmallInteger(1)
        self.w_two = model.W_SmallInteger(2)
        w_special_selectors = model.W_PointersObject(self,
            self.classtable['w_Array'], len(constants.SPECIAL_SELECTORS) * 2)
        self.w_special_selectors = w_special_selectors

        self.objtable = {}
        for name in constants.objects_in_special_object_table:
            name = "w_" + name
            try:
                self.objtable[name] = locals()[name]
            except KeyError, e:
                self.objtable[name] = None
    

def bootstrap_class(space, instsize, w_superclass=None, w_metaclass=None,
                    name='?', format=shadow.POINTERS, varsized=False):
    w_class = model.W_PointersObject(space, w_metaclass, 0)
    s = instantiate(shadow.ClassShadow)
    s.space = space
    s.version = version.Version()
    s._w_self = w_class
    s.subclass_s = {}
    s._s_superclass = None
    s.store_w_superclass(w_superclass)
    s.name = name
    s._instance_size = instsize
    s.instance_kind = format
    s._s_methoddict = None
    s.instance_varsized = varsized or format != shadow.POINTERS
    w_class.store_shadow(s)
    return w_class
