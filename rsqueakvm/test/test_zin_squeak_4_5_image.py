import sys

from rsqueakvm.model.numeric import W_SmallInteger
from rsqueakvm.model.pointers import W_PointersObject, W_FixedPointersObject

from .util import read_image, copy_to_module, cleanup_module, slow_test, very_slow_test, skip


# The tests are quick, but loading the big image takes time.
pytestmark = slow_test

# Otherwise the image does not load on some systems.
# This is not required when using pypy.
sys.setrecursionlimit(100000)

def setup_module():
    space, interp, image, reader = read_image('Squeak4.3.image')
    w = space.w
    find_symbol = space.find_symbol_in_methoddict
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def test_all_pointers_are_valid():
    from test_miniimage import _test_all_pointers_are_valid
    _test_all_pointers_are_valid(reader)

@very_slow_test
def test_lookup_abs_in_integer():
    from test_miniimage import _test_lookup_abs_in_integer
    _test_lookup_abs_in_integer(interp)

def test_ensure():
    #ensure
    #    [^'b1'] ensure: [^'b2']

    ensure_ = find_symbol("ensure:", space.w_BlockClosure)
    bytes = [0x8F, 0, 0, 2, 0x21, 0x7c,
               0x8F, 0, 0, 2, 0x22, 0x7c,
               0xe0, 0x87, 0x78]

    w_method = space.make_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])
    result = interp.execute_method(w_method)
    assert result.unwrap_string(None) == 'b2'

def test_ensure_save_original_nlr():
    #ensure
    #    [^'b1'] ensure: ['b2']

    ensure_ = find_symbol("ensure:", space.w_BlockClosure)
    bytes = [0x8F, 0, 0, 2, 0x21, 0x7c,
               0x8F, 0, 0, 2, 0x22, 0x7d,
               0xe0, 0x87, 0x78]

    w_method = space.make_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])
    result = interp.execute_method(w_method)
    assert result.unwrap_string(None) == 'b1'

def test_ContextPart_jump():
    """
    Code: Create a Block context that jumps back to its sender, instead of returning normally.
    The Block is not executed to the end, the sender chain is manipulated.
    The local variable should be the value pushed on the sender context before jumping to it.
    a := 5.
    a := [ thisContext sender push: 2. thisContext sender jump. 10 ] value.
    ^ a
    """
    ContextPart = space.w_MethodContext.as_class_get_shadow(space).s_superclass().w_self()
    push = find_symbol("push:", ContextPart)
    sender = find_symbol("sender", ContextPart)
    jump = find_symbol("jump", ContextPart)

    bytes = [0x21, 0x82, 0xc0, # Set a
           0x8f, 0x00, 0x00, 0x0b, # Push block
                0x89, 0xd3, # Send sender
                0x77, 0xe2, # Send push
                0x87, 0x89, 0xd3, 0xd4, # Send jump
                0x87, 0x25, 0x7d, # Block rest (not executed)
           0xc9, 0x82, 0xc0, 0x40, 0x7c] # Send value and return

    Association = space.w_Point # Wrong class, doesn't matter.
    assoc = W_FixedPointersObject(space, Association, 2)
    assoc.store(space, 0, w('a'))
    assoc.store(space, 1, w(3))
    w_method = space.make_method(bytes, [assoc, w(5), push, sender, jump, w(10)])
    result = interp.execute_method(w_method)
    assert isinstance(result, W_SmallInteger)
    assert result.value == 2

def test_ContextPart_jump_nonlocal():
    """
    Like above test, but with three blocks to make the return non-local.
    Also, store the outer context beforehand.
    a := 5.
    outer := thisContext.
    a := [[[ outer push: 2. outer jump. 10 ] value ] value] value.
    ^ a
    """
    ContextPart = space.w_MethodContext.as_class_get_shadow(space).s_superclass().w_self()
    push = find_symbol("push:", ContextPart)
    jump = find_symbol("jump", ContextPart)

    bytes = [0x21, 0x82, 0xc0, # Set a
               0x89, 0x82, 0xc2, # Set outer
               0x8f, 0x00, 0x00, 0x15, # Push block
                    0x8f, 0x00, 0x00, 0x0f, # Push block
                        0x8f, 0x00, 0x00, 0x09, # Push block
                            0x42, 0x77, 0xe3, # Push 2
                            0x87, 0x42, 0xd4, # Send jump
                            0x87, 0x25, 0x7d, # Block rest (not executed)
                        0xc9, 0x7d, # Send value and return
                    0xc9, 0x7d, # Send value and return
               0xc9, 0x82, 0xc0, 0x40, 0x7c] # Send value and return

    Association = space.w_Point # Wrong class, doesn't matter.
    assoc = W_FixedPointersObject(space, Association, 2)
    assoc.store(space, 0, w('a'))
    assoc.store(space, 1, space.w_nil)
    assoc2 = W_FixedPointersObject(space, Association, 2)
    assoc2.store(space, 0, w('outer'))
    assoc2.store(space, 1, space.w_nil)
    w_method = space.make_method(bytes, [assoc, w(5), assoc2, push, jump, w(10)])
    result = interp.execute_method(w_method)
    assert isinstance(result, W_SmallInteger)
    assert result.value == 2

def test_contextOn_do_():
    """
    contextOn:do: is some very heavy meta programming. It creates and returns a separate stack frame,
    settings it's sender to nil, thereby manipulating the senders of two contexts.
    The Point in there should actually be UnhandledError or something.
    The test here is just that this works.
    ctx := ContextPart contextOn: Point do: ['nothing']
    """
    ContextPart = space.w_MethodContext.as_class_get_shadow(space).s_superclass().w_self()
    ContextPartClass = ContextPart.getclass(space).as_class_get_shadow(space).w_self()
    contextOnDo = find_symbol("contextOn:do:", ContextPartClass)

    bytes = [
        0x42, 0x43, # Push the classes
        0x8f, 0x00, 0x00, 0x02, # Push block,
            0x24, 0x7d, # in the block
        0xf1, 0x81, 0xc0, 0x7c # Send contextOn:do:
    ]

    Association = space.w_Point # Wrong class, doesn't matter.
    ctxAssoc = W_FixedPointersObject(space, Association, 2)
    ctxAssoc.store(space, 0, w('ctx'))
    ctxAssoc.store(space, 1, space.w_nil)
    contextPartAssoc = W_FixedPointersObject(space, Association, 2)
    contextPartAssoc.store(space, 0, w('ContextPart'))
    contextPartAssoc.store(space, 1, ContextPart)
    errorAssoc = W_FixedPointersObject(space, Association, 2)
    errorAssoc.store(space, 0, w('Point'))
    errorAssoc.store(space, 1, Association)
    w_method = space.make_method(bytes, [ctxAssoc, contextOnDo, contextPartAssoc, errorAssoc, w('nothing')])

    result = interp.execute_method(w_method)
    assert isinstance(result, W_PointersObject)
    s = result.as_context_get_shadow(space)
    assert s.w_method().lookup_selector == "on:do:"
    assert s.w_method().primitive() == 199
    assert s.s_sender() == None

@skip('Hangs')
def test_semaphore():
    w_semaphore_cls = space.w_timerSemaphore().getclass(space)
    w_sema = image.find_symbol(space, reader, "Semaphore")
    w_fork = image.find_symbol(space, reader, "fork")
    w_wait = image.find_symbol(space, reader, "wait")
    w_yield = image.find_symbol(space, reader, "yield")
    w_processor = space.w_schedulerassociationpointer
    w_suspPrimOFail = image.find_symbol(space, reader, "suspendPrimitivelyOrFail")

    bytes = [
        0x40, # pushLit: Semaphore
        0xCC, # send: new
        0x6A, # popIntoTemp: 2
        0x12, # pushTemp: 2
        0x8F, 0x10, 0x00, 0x03, # closureNumCopied: 1 numArgs: 0 bytes 49 to 51
        0x10, # pushTemp: 0
        0xD2, # send: wait
        0x7D, # blockReturn
        0xD1, # send: fork
        0x69, # popIntoTemp: 1
        0x44, # pushLit: Processor
        0xD3, # send: yield
        0x87, # pop
        0x11, # pushTemp: 1
        0xD5, # send: suspendPrimitivelyOrFail
        0x68, # popIntoTemp: 0
        0x10, # pushTemp: 0
        0x7C, # returnTop
    ]

    Association = space.w_Point # Wrong class, doesn't matter.
    semaAssoc = W_PointersObject(space, Association, 2)
    semaAssoc.store(space, 0, w_sema)
    semaAssoc.store(space, 1, w_semaphore_cls)
    w_method = space.make_method(bytes, [semaAssoc, w_fork, w_wait, w_yield, w_processor, w_suspPrimOFail, w('nothing')])

    result = interp.execute_method(w_method)
    assert isinstance(result, W_PointersObject)
