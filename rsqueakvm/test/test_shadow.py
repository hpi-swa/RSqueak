import pytest
import random

from rsqueakvm import storage_classes, constants, wrapper
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.model.pointers import W_PointersObject, W_FixedPointersObject
from rsqueakvm.model.variable import W_BytesObject

from .util import create_space, copy_to_module, cleanup_module

@pytest.fixture
def space_v3():
    return create_space(bootstrap = True)

@pytest.fixture
def space_spur():
    space = create_space(bootstrap = True)
    space.is_spur.activate()
    return space

def setup_module():
    space = space_v3()
    w_Object     = space.w_Object
    w_Metaclass  = space.w_Metaclass
    w_MethodDict = space.w_MethodDict
    w_Array      = space.w_Array
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def build_methoddict(methods):
    size = int(len(methods) * 1.5)
    w_methoddict = w_MethodDict.as_class_get_shadow(space).new(size)
    w_array = w_Array.as_class_get_shadow(space).new(size)
    for i in range(size):
        w_array.store(space, i, space.w_nil)
        w_methoddict.store(space, constants.METHODDICT_NAMES_INDEX+i, space.w_nil)
    w_tally = space.wrap_int(len(methods))
    w_methoddict.store(space, constants.METHODDICT_TALLY_INDEX, w_tally)
    w_methoddict.store(space, constants.METHODDICT_VALUES_INDEX, w_array)
    positions = range(size)
    random.shuffle(positions)
    for selector, w_compiledmethod in methods.items():
        pos = positions.pop()
        w_selector = space.wrap_string(selector)
        w_methoddict.store(space, constants.METHODDICT_NAMES_INDEX+pos, w_selector)
        w_array.store(space, pos, w_compiledmethod)
    return w_methoddict

def build_smalltalk_class(name, format, w_superclass=None,
                          w_classofclass=None, methods={}, space=None):
    if space is None:
        space = globals()["space"]
    if w_superclass is None:
        w_superclass = w_Object
    if w_classofclass is None:
        w_classofclass = build_smalltalk_class(None, 0x94,
                                               w_superclass.getclass(space),
                                               w_Metaclass)
    w_methoddict = build_methoddict(methods)
    size = constants.CLASS_NAME_INDEX + 1
    w_class = W_FixedPointersObject(space, w_classofclass, size)
    w_class.store(space, constants.CLASS_SUPERCLASS_INDEX, w_superclass)
    w_class.store(space, constants.CLASS_METHODDICT_INDEX, w_methoddict)
    w_class.store(space, constants.CLASS_FORMAT_INDEX, space.wrap_int(format))
    if name is not None:
        w_class.store(space, constants.CLASS_NAME_INDEX, space.wrap_string(name))
    w_class.as_class_get_shadow(space).s_methoddict().sync_method_cache()
    return w_class

def basicshape(name, format, kind, varsized, instsize):
    w_class = build_smalltalk_class(name, format)
    classshadow = w_class.as_class_get_shadow(space)
    assert classshadow.instance_kind == kind
    assert classshadow.isvariable() == varsized
    assert classshadow.instsize() == instsize
    assert classshadow.name == name
    assert classshadow.s_superclass() is w_Object.as_class_get_shadow(space)

def test_basic_shape():
    yield basicshape, "Empty",        0x02,    storage_classes.POINTERS, False, 0
    yield basicshape, "Seven",        0x90,    storage_classes.POINTERS, False, 7
    yield basicshape, "Seventyseven", 0x1009C, storage_classes.POINTERS, False, 77
    yield basicshape, "EmptyVar",     0x102,   storage_classes.POINTERS, True,  0
    yield basicshape, "VarTwo",       0x3986,  storage_classes.POINTERS, True,  2
    yield basicshape, "VarSeven",     0x190,   storage_classes.POINTERS, True,  7
    yield basicshape, "Bytes",        0x402,   storage_classes.BYTES,    True,  0
    yield basicshape, "Words",        0x302,   storage_classes.WORDS,    True,  0
    yield basicshape, "CompiledMeth", 0xE02,   storage_classes.COMPILED_METHOD, True, 0

def test_methoddict():
    methods = {'foo': W_PreSpurCompiledMethod(space, 0),
               'bar': W_PreSpurCompiledMethod(space, 0)}
    w_class = build_smalltalk_class("Demo", 0x90, methods=methods)
    classshadow = w_class.as_class_get_shadow(space)
    methoddict = classshadow.s_methoddict().methoddict
    assert len(methods) == len(methoddict)
    for w_key, value in methoddict.items():
        assert methods[w_key.unwrap_string(None)] is value

def create_method(tempsize=3,argsize=2, bytes="abcde"):
    w_m = W_PreSpurCompiledMethod(space, )
    w_m.bytes = bytes
    w_m._tempsize = tempsize
    w_m.argsize = argsize
    w_m.literalsize = 2
    w_m.update_frame_size()
    return w_m

def methodcontext(w_sender=None, pc=13, stackpointer=0, stacksize=5,
                  method=None):
    if w_sender is None:
        w_sender = space.w_nil
    if method is None:
        method = create_method()
    w_object = W_PointersObject(space, space.w_MethodContext, constants.MTHDCTX_TEMP_FRAME_START+method.tempsize()+stacksize)
    w_object.store(space, constants.CTXPART_SENDER_INDEX, w_sender)
    w_object.store(space, constants.CTXPART_PC_INDEX, space.wrap_int(pc))
    w_object.store(space, constants.CTXPART_STACKP_INDEX, space.wrap_int(method.tempsize()+stackpointer))
    w_object.store(space, constants.MTHDCTX_METHOD, method)
    # XXX
    w_object.store(space, constants.MTHDCTX_CLOSURE_OR_NIL, space.w_nil)
    w_object.store(space, constants.MTHDCTX_RECEIVER, space.wrap_string('receiver'))

    w_object.store(space, constants.MTHDCTX_TEMP_FRAME_START, space.wrap_string('el'))
    return w_object

def blockcontext(w_sender=None, pc=13, stackpointer=1, stacksize=5,
                  home=None):
    if w_sender is None:
        w_sender = space.w_nil
    if home is None:
        home = methodcontext()
    w_object = W_PointersObject(space, space.w_BlockContext, constants.MTHDCTX_TEMP_FRAME_START+stacksize)
    w_object.store(space, constants.CTXPART_SENDER_INDEX, w_sender)
    w_object.store(space, constants.CTXPART_PC_INDEX, space.wrap_int(pc))
    w_object.store(space, constants.CTXPART_STACKP_INDEX, space.wrap_int(stackpointer))
    w_object.store(space, constants.BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX, space.wrap_int(54))
    w_object.store(space, constants.BLKCTX_INITIAL_IP_INDEX, space.wrap_int(17))
    w_object.store(space, constants.BLKCTX_HOME_INDEX, home)
    w_object.store(space, constants.BLKCTX_STACK_START, space.wrap_string('el'))
    return w_object

def test_context():
    w_m = create_method()
    w_object = methodcontext(stackpointer=3, method=w_m)
    w_object2 = methodcontext(w_sender=w_object)
    s_object = w_object.as_context_get_shadow(space)
    assert len(s_object.stack()) == 3
    s_object2 = w_object2.as_context_get_shadow(space)
    assert w_object2.fetch(space, constants.CTXPART_SENDER_INDEX) == w_object
    assert s_object.w_self() == w_object
    assert s_object2.w_self() == w_object2
    assert s_object.s_sender() == None
    assert s_object2.s_sender() == s_object
    assert s_object.w_receiver().unwrap_string(None) == 'receiver'
    s_object2.settemp(0, 'a')
    s_object2.settemp(1, 'b')
    assert s_object2.gettemp(1) == 'b'
    assert s_object2.gettemp(0) == 'a'
    assert s_object.w_method() == w_m
    idx = s_object.stackstart() + s_object.tempsize()
    w_object.store(space, idx, space.wrap_string('f'))
    w_object.store(space, idx + 1, space.wrap_string('g'))
    w_object.store(space, idx + 2, space.wrap_string('h'))
    assert map(lambda x: x.unwrap_string(None), s_object.stack()) == ['f', 'g', 'h' ]
    assert s_object.top().unwrap_string(None) == 'h'
    s_object.push('i')
    assert s_object.top() == 'i'
    assert s_object.peek(1).unwrap_string(None) == 'h'
    assert s_object.pop() == 'i'
    assert map(lambda x: x.unwrap_string(None), s_object.pop_and_return_n(2)) == ['g', 'h']
    assert s_object.pop().unwrap_string(None) == 'f'
    assert s_object.external_stackpointer() == s_object.stackstart() + s_object.tempsize()
    assert s_object.stackdepth() == s_object.tempsize()

def test_methodcontext():
    w_m = create_method()
                              # Point over 2 literals of size 4
    w_object = methodcontext(pc=13,method=w_m)
    s_object = w_object.as_context_get_shadow(space)
    assert s_object.fetch_next_bytecode() == 97
    assert s_object.fetch_next_bytecode() == 98
    assert s_object.fetch_next_bytecode() == 99
    assert s_object.fetch_next_bytecode() == 100
    assert s_object.fetch_next_bytecode() == 101
    assert s_object.s_home() == s_object

def assert_contains_nils(w_obj):
    for i in range(w_obj.size()):
        assert w_obj.fetch(space, i).is_nil(space)

def test_attach_mc():
    w_m = create_method()
    w_object = methodcontext(pc=13, method=w_m)
    s_object = w_object.as_context_get_shadow(space)
    assert s_object.fetch(w_object, 1).value == 13

def test_attach_bc():
    w_object = blockcontext(pc=13)
    s_object = w_object.as_context_get_shadow(space)
    assert s_object.fetch(w_object, 1).value == 13

def test_replace_to_bc():
    w_object = blockcontext(pc=13)
    s_object = w_object.as_context_get_shadow(space)
    s_object.strategy = None
    s_newobject = w_object.as_context_get_shadow(space)
    assert ([s_newobject.own_fetch(i) for i in range(s_newobject.own_size())] ==
            [s_object.own_fetch(i) for i in range(s_newobject.own_size())])
    assert w_object.strategy is s_newobject
    assert s_object.own_fetch(1).value == 13

def test_cached_object_shadow():
    l = map(space.w, [0, 1, 2, 3, 4, 5, 6, 7])
    w_o = space.wrap_list(l)
    s_o = w_o.as_cached_object_get_shadow(space)
    version = s_o.version
    for i in range(w_o.size()):
        assert w_o.at0(space, i) == l[i]
    w_o.atput0(space, 0, 8)
    assert version is not s_o.version
    assert w_o.at0(space, 0) == 8

def test_observee_shadow():
    notified = False
    class Observer():
        def __init__(self): self.notified = False
        def notify(self): self.notified = True
    o = Observer()
    w_o = w_Array.as_class_get_shadow(space).new(1)
    w_o.as_observed_get_shadow(space).set_observer(o)
    assert not o.notified
    w_o.store(space, 0, 1)
    assert o.notified
    assert w_o.fetch(space, 0) == 1
    try:
        w_o.strategy.set_observer(Observer())
    except RuntimeError:
        pass
    else:
        assert False

def test_cached_methoddict():
    # create a methoddict
    foo = W_PreSpurCompiledMethod(space, 0)
    bar = W_PreSpurCompiledMethod(space, 0)
    baz = W_PreSpurCompiledMethod(space, 0)
    methods = {'foo': foo,
               'bar': bar}
    w_class = build_smalltalk_class("Demo", 0x90, methods=methods)
    s_class = w_class.as_class_get_shadow(space)
    s_methoddict = s_class.s_methoddict()
    s_methoddict.sync_method_cache()
    i = 0
    key = s_methoddict.w_self().fetch(s_methoddict.space, constants.METHODDICT_NAMES_INDEX+i)
    while key.is_nil(space):
        i = i + 1
        key = s_methoddict.w_self().fetch(s_methoddict.space, constants.METHODDICT_NAMES_INDEX+i)

    assert (s_class.lookup(key) is foo
            or s_class.lookup(key) is bar)
    # change that entry
    w_array = s_class.w_methoddict().fetch(s_class.space, constants.METHODDICT_VALUES_INDEX)
    version = s_class.version
    w_array.atput0(space, i, baz)

    assert s_class.lookup(key) is baz
    assert version is not s_class.version

def test_updating_class_changes_subclasses():
    w_parent = build_smalltalk_class("Demo", 0x90,
            methods={'bar': W_PreSpurCompiledMethod(space, 0)})
    w_class = build_smalltalk_class("Demo", 0x90,
            methods={'foo': W_PreSpurCompiledMethod(space, 0)}, w_superclass=w_parent)
    s_class = w_class.as_class_get_shadow(space)
    version = s_class.version

    w_method = W_PreSpurCompiledMethod(space, 0)
    key = space.wrap_string('foo')

    s_md = w_parent.as_class_get_shadow(space).s_methoddict()
    s_md.sync_method_cache()
    w_ary = s_md._w_self.fetch(s_md.space, constants.METHODDICT_VALUES_INDEX)
    s_md._w_self.atput0(space, 0, key)
    w_ary.atput0(space, 0, w_method)

    assert s_class.lookup(key) is w_method
    assert s_class.version is not version
    assert s_class.version is w_parent.as_class_get_shadow(space).version

def test_returned_contexts_pc():
    w_context = methodcontext()
    s_context = w_context.as_context_get_shadow(space)
    assert not w_context.fetch(space, constants.CTXPART_PC_INDEX).is_nil(space)
    s_context.mark_returned()
    assert w_context.fetch(space, constants.CTXPART_PC_INDEX).is_nil(space)

def test_methodcontext_s_home():
    w_context = methodcontext()
    s_context = w_context.as_context_get_shadow(space)

    w_closure = space.newClosure(w_context, 3, 0, [])
    s_closure_context = w_closure.create_frame(space)
    assert s_closure_context.s_home() is s_context

def test_class_format_v3(space_v3):
    """
    <2 bits=instSize//64><5 bits=cClass><4 bits=instSpec><6 bits=instSize\\64><1 bit=0>
    """
    from rsqueakvm.test.test_model import joinbits
    instsize_low = 3
    instsize_high = 2
    inst_spec = 2 # array format
    compact_class_index = 0 # ignored
    def assert_shadow_properties(instsize, is_variable):
        format = joinbits([0, instsize_low, inst_spec, compact_class_index, instsize_high],
                [1,6,4,5,2])
        w_class = build_smalltalk_class('TestClass', format)
        s_class = w_class.as_class_get_shadow(space_v3)
        assert s_class.instsize() == instsize
        assert s_class.isvariable() == is_variable
    assert_shadow_properties(instsize = 3 + 2*64 - 1, is_variable = True)
    inst_spec = 1 # fixed fields only
    instsize_low = 6
    instsize_high = 0
    assert_shadow_properties(instsize = 6 - 1, is_variable = False)

def test_class_format_spur(space_spur):
    """
    <5 bits inst spec><16 bits inst size>
    """
    assert space_spur.is_spur.is_set()
    from rsqueakvm.test.test_model import joinbits
    instsize = 3
    inst_spec = 2 # array format
    def assert_shadow_properties(expected_instsize, is_variable):
        format = joinbits([instsize, inst_spec], [16, 5])
        w_class = build_smalltalk_class('TestClass', format, space=space_spur)
        s_class = w_class.as_class_get_shadow(space_spur)
        assert s_class.instsize() == expected_instsize
        assert s_class.isvariable() == is_variable
    assert_shadow_properties(expected_instsize = 3, is_variable = True)
    inst_spec = 1 # fixed fields only
    instsize = 560
    assert_shadow_properties(expected_instsize = 560, is_variable = False)

def test_class_new_fixed_v3(space_v3):
    w_class = build_smalltalk_class('TestClass', (4 << 1) | (1 << 7))  # fixed format
    w_inst = w_class.as_class_get_shadow(space_v3).new()
    assert isinstance(w_inst, W_PointersObject)
    assert w_inst.size() == 3
def test_class_new_array_v3(space_v3):
    w_class = build_smalltalk_class('TestClass', 2 << 7)  # array format
    w_inst = w_class.as_class_get_shadow(space_v3).new()
    assert isinstance(w_inst, W_PointersObject)
def test_class_new_bytes_v3(space_v3):
    w_class = build_smalltalk_class('TestClass', (1 << 1) | (8 << 7))  # indexable bytes
    w_inst = w_class.as_class_get_shadow(space_v3).new(5)
    assert isinstance(w_inst, W_BytesObject)
    assert w_inst.size() == 5
def test_class_new_compiledmethod_v3(space_v3):
    w_class = build_smalltalk_class('TestClass', (1 << 1) | (12 << 7))
    w_inst = w_class.as_class_get_shadow(space_v3).new(4)
    assert isinstance(w_inst, W_PreSpurCompiledMethod)
    assert w_inst.size() == 8

def test_class_new_fixed_spur(space_spur):
    w_class = build_smalltalk_class('TestClass', 4 | (1 << 16), space=space_spur)
    w_inst = w_class.as_class_get_shadow(space_spur).new()
    assert isinstance(w_inst, W_PointersObject)
    assert w_inst.size() == 4
def test_class_new_array_spur(space_spur):
    w_class = build_smalltalk_class('TestClass', 2 << 16, space=space_spur)
    w_inst = w_class.as_class_get_shadow(space_spur).new()
    assert isinstance(w_inst, W_PointersObject)
def test_class_new_bytes_spur(space_spur):
    w_class = build_smalltalk_class('TestClass', 16 << 16, space=space_spur)
    w_inst = w_class.as_class_get_shadow(space_spur).new(5)
    assert isinstance(w_inst, W_BytesObject)
    assert w_inst.size() == 5
def test_class_new_compiledmethod_spur(space_spur):
    w_class = build_smalltalk_class('TestClass', 24 << 16, space=space_spur)
    w_inst = w_class.as_class_get_shadow(space_spur).new(4)
    assert isinstance(w_inst, W_SpurCompiledMethod)
    assert w_inst.size() == 8

def test_class_of_smallinteger(space_spur):
    w_SmallInteger = space_spur.w_SmallInteger
    w_SmallInteger.store(space_spur, constants.CLASS_FORMAT_INDEX, space_spur.wrap_int(7 << 16))
    # SmallInteger and Character have 7 (forwarder) as their class format
    # which is invalid and further prevents instantiation
