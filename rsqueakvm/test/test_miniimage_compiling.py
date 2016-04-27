from rsqueakvm import constants, storage_classes
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_LargePositiveInteger1Word
from rsqueakvm.model.variable import W_BytesObject

from .util import read_image, open_reader, copy_to_module, cleanup_module, TestInterpreter, slow_test, very_slow_test


def setup_module():
    space, interp, _, _ = read_image("mini.image")
    w = space.w
    def perform_wrapper(receiver, selector, *args):
        w_selector = None if isinstance(selector, str) else selector
        return interp.perform(receiver, selector, w_selector, list(args))
    perform = perform_wrapper
    copy_to_module(locals(), __name__)
    space.simulate_numeric_primitives.activate()


def teardown_module():
    cleanup_module(__name__)

# ------ tests ------------------------------------------

def test_load_image():
    pass

@very_slow_test
def test_make_new_class():
    sourcecode = """makeNewClass
        ^ Object
                subclass: #MySubForm
                instanceVariableNames: 'clippingBox '
                classVariableNames: 'ScreenSave '
                poolDictionaries: ''
                category: 'Graphics-Display Objects'"""
    perform(w(0).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_res = perform(w(0), "makeNewClass")
    assert isinstance(w_res.strategy, storage_classes.ClassShadow)
    assert w_res.strategy.name == "MySubForm"
    assert w_res.strategy._instance_size == 1

@very_slow_test
def test_change_class_layout():
    sourcecode = """makeChangedClass
^ MessageSet subclass: #ChangedMessageSet
        instanceVariableNames: 'changeSet uselessVar'
        classVariableNames: ''
        poolDictionaries: ''
        category: 'Interface-Browser'
"""
    perform(w(0).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_res = perform(w(0), "makeChangedClass")
    assert w_res.strategy.name == "ChangedMessageSet"
    assert w_res.strategy._instance_size == 15

@very_slow_test
def test_become_one_way():
    sourcecode = """objectsForwardIdentityTo: to
        <primitive: 72>"""
    perform(space.w_Array, "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    sourcecode = """doIt
        | from to oldthing newthing |
        Object subclass: #OldThing
               instanceVariableNames: ''
               classVariableNames: ' '
               poolDictionaries: ''
               category: 'Pypy'.
        Object subclass: #NewThing
               instanceVariableNames: 'otherThing'
               classVariableNames: ''
               poolDictionaries: ''
               category: 'Pypy'.
        oldthing := (Smalltalk at: #OldThing) new.
        newthing := (Smalltalk at: #NewThing) new.
        newthing instVarAt: 1 put: oldthing.
        from := Array with: oldthing.
        to := Array with: newthing.
        from objectsForwardIdentityTo: to.
        ^ Array with: (from at: 1) with: (to at: 1) with: oldthing with: newthing
    """
    perform(w(0).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    res_w = space.unwrap_array(perform(w(0), "doIt"))
    assert res_w[0].class_shadow(space).name == "NewThing"
    assert res_w[0].fetch(space, 0) is res_w[0]
    assert res_w[0] is res_w[1]
    assert res_w[0] is res_w[2]
    assert res_w[0] is res_w[3]

def test_compile_method():
    sourcecode = """fib
                        ^self < 2
                            ifTrue: [ 1 ]
                            ifFalse: [ (self - 1) fib + (self - 2) fib ]"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    assert perform(w(10), "fib").is_same_object(w(89))

def test_allInstances_in_context():
    sourcecode = """aFraction
    | a |
    a := 5 asInteger.
    a := a / 42 asInteger.
    ^ Fraction allInstances"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_result = perform(w(10), "aFraction")
    result_w = space.unwrap_array(w_result)
    assert len(result_w) == 1
    pointers_w = result_w[0].fetch_all(space)
    assert pointers_w[0].value == 5
    assert pointers_w[1].value == 42

def test_become():
    sourcecode = """
    testBecome
      | p1 p2 a |
      p1 := 1@2.
      p2 := #(3 4 5).
      a := p1 -> p2.
      (1@2 = a key)        ifFalse: [^1].
      (#(3 4 5) = a value) ifFalse: [^2].
      (p1 -> p2 = a)       ifFalse: [^3].
      (p1 == a key)        ifFalse: [^4].
      (p2 == a value)      ifFalse: [^5].
      p1 become: p2.
      (1@2 = a value)      ifFalse: [^6].
      (3 = (a key at: 1))  ifFalse: [^7].
      (4 = (a key at: 2))  ifFalse: [^8].
      (5 = (a key at: 3))  ifFalse: [^9].
      (p1 -> p2 = a)       ifFalse: [^10].
      (p1 == a key)        ifFalse: [^11].
      (p2 == a value)      ifFalse: [^12].

      ^42"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_result = perform(w(10), "testBecome")
    assert space.unwrap_int(w_result) == 42

def test_cached_methoddict():
    sourcecode = """fib
                        ^self < 2
                            ifTrue: [ 1 ]
                            ifFalse: [ ((self - 1) fib + (self - 2) fib) + 1 ]"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    assert perform(w(5), "fib").is_same_object(w(15))
    sourcecode = """fib
                        ^self < 2
                            ifTrue: [ 1 ]
                            ifFalse: [ (self - 1) fib + (self - 2) fib ]"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    assert perform(w(10), "fib").is_same_object(w(89))

def test_compiling_float():
    sourcecode = """aFloat
                        ^ 1.1"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_result = perform(w(10), "aFloat")
    assert isinstance(w_result, W_Float)
    assert w_result.value == 1.1

def test_compiling_large_positive_integer():
    sourcecode = """aLargeInteger
                        ^ 16rFFFFFFFF"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_result = perform(w(10), "aLargeInteger")
    if not constants.IS_64BIT:
        assert isinstance(w_result, W_LargePositiveInteger1Word)
    else:
        assert isinstance(w_result, W_SmallInteger)

def test_compiling_large_large_positive_integer():
    sourcecode = """aLargeInteger
                        ^ 16rFFFFFFFFFFFFFFFF"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    w_result = perform(w(10), "aLargeInteger")
    if not constants.IS_64BIT:
        assert isinstance(w_result, W_BytesObject)
    else:
        assert isinstance(w_result, W_LargePositiveInteger1Word)

def test_simulate_numericprim():
    sourcecode = """absentPrimitive: anInt with: anotherInt
        <primitive: 98>
        ^'numeric fallback for ', anInt asString, ' ', anotherInt asString"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    sourcecode = """simulatePrimitive: aPrimitive args: args
        ^'numeric simulation for ', args first asString, ' ', args second asString"""
    w_sim = perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    # XXX the lookup for that selector is static so the simulation lookup would be failing
    interp.image.w_simulatePrimitive = w_sim

    w_result = perform(w(10), "absentPrimitive:with:", w(3), w(4))
    assert isinstance(w_result, W_BytesObject)
    assert w_result.unwrap_string(space) == 'numeric simulation for 3 4'

def test_simulate_numericprim_fallback():
    sourcecode = """absentPrimitive: anInt with: anotherInt
        |errorCode|
        <primitive: 98> "error: errorCode> is not implemented in the mini.image yet"
        ^'numeric fallback for ', anInt asString, ' ', anotherInt asString, ' because of ', errorCode asString"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    sourcecode = """metaPrimFailed: errorCode
        <primitive: 255>"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    sourcecode = """simulatePrimitive: aPrimitive args: args
        ^self metaPrimFailed: 123"""
    w_sim = perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    # XXX the lookup for that selector is static so the simulation lookup would be failing
    interp.image.w_simulatePrimitive = w_sim

    w_result = perform(w(10), "absentPrimitive:with:", w(3), w(4))
    assert isinstance(w_result, W_BytesObject)
    assert w_result.unwrap_string(space) == 'numeric fallback for 3 4 because of 123'

def test_simulate_externalcall():
    sourcecode = """absentPrimitive: anInt with: anotherInt
        | externalCallTarget |
        "do not use <primitive: 'primitiveSimulation' module: 'MyPlugin'> as mini.image doesn't have that yet"
        <primitive: 117>
        externalCallTarget := #(MyPlugin primitiveSimulation).
        ^'externalcall fallback for ', anInt asString, ' ', anotherInt asString"""
    perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))
    sourcecode = """simulatePrimitive: aPrimitive args: args
        ^'externalcall simulation for ', args first asString, ' ', args second asString"""
    w_sim = perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

    # XXX the lookup for that selector is static so the simulation lookup would be failing
    interp.image.w_simulatePrimitive = w_sim

    w_result = perform(w(10), "absentPrimitive:with:", w(3), w(4))
    assert isinstance(w_result, W_BytesObject)
    assert w_result.unwrap_string(space) == 'externalcall simulation for 3 4'

def test_snapshotPrimitive(tmpdir):
    newname = str(tmpdir.join("test_snapshot.image"))
    space, interp, _, _ = read_image("mini.image")
    def perform(receiver, selector, *args):
        w_selector = None if isinstance(selector, str) else selector
        return interp.perform(receiver, selector, w_selector, list(args))
    space.simulate_numeric_primitives.activate()
    space.set_system_attribute(constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, newname)
    w_result = perform(space.special_object("w_smalltalkdict"), "snapshotPrimitive")
    assert w_result is space.w_false
    space2, interp2, image2, reader2 = read_image(newname)
    for f,n in {
            'w_true': 'True', 'w_false': 'False', 'w_nil': 'UndefinedObject'
    }.iteritems():
        assert getattr(space, f).getclass(space).as_class_get_shadow(space).name == getattr(space2, f).getclass(space2).as_class_get_shadow(space).name
    for f in [
            'w_doesNotUnderstand',
            'w_mustBeBoolean'
    ]:
        assert space.unwrap_string(space.objtable[f]) == space2.unwrap_string(space2.objtable[f])
