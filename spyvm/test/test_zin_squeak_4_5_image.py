from spyvm import squeakimage, model, constants, interpreter, shadow, objspace
from .util import read_image, find_symbol_in_methoddict_of, copy_to_module, cleanup_module

import operator

def setup_module():
    space, interp, image, reader = read_image('Squeak4.5-12568.image')
    w = space.w
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def test_all_pointers_are_valid():
    from test_miniimage import _test_all_pointers_are_valid
    from test_miniimage import _test_lookup_abs_in_integer
    _test_all_pointers_are_valid(reader)
    _test_lookup_abs_in_integer(interp)

def create_method(bytes, literals=[], islarge=0, argsize=0, tempsize=0):
    w_method = model.W_CompiledMethod(space, len(bytes))
    w_method.bytes = bytes
    w_method.islarge = islarge
    w_method.argsize = argsize
    w_method._tempsize = tempsize

    w_method.setliterals(literals)
    return w_method

def find_symbol(w_class, symbolname):
    s_class = w_class.as_class_get_shadow(space)
    symbol = find_symbol_in_methoddict_of(symbolname, s_class)
    assert symbol is not None, 'Using image without %s method.' % symbolname
    return symbol
    
def execute_frame(w_method):
    # create a frame for our newly crafted method with a valid sender (to avoid raising returnFromTop to early)
    s_initial_frame = create_method(chr(0x7c)).create_frame(space, w(0), [])
    s_frame = w_method.create_frame(space, w(0))
    s_frame.store_s_sender(s_initial_frame)
    
    try:
        interp.loop(s_frame.w_self())
    except interpreter.ReturnFromTopLevel, e:
        return e.object
    
def test_ensure():
    #ensure
    #    [^'b1'] ensure: [^'b2']
    
    ensure_ = find_symbol(space.w_BlockClosure, "ensure:")
    bytes = reduce(operator.add, map(chr, [0x8F, 0, 0, 2, 0x21, 0x7c,
                                           0x8F, 0, 0, 2, 0x22, 0x7c,
                                           0xe0, 0x87, 0x78]))
    
    w_method = create_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])
    result = execute_frame(w_method)
    assert result.as_string() == 'b2'

def test_ensure_save_original_nlr():
    #ensure
    #    [^'b1'] ensure: ['b2']
    
    ensure_ = find_symbol(space.w_BlockClosure, "ensure:")
    bytes = reduce(operator.add, map(chr, [0x8F, 0, 0, 2, 0x21, 0x7c,
                                           0x8F, 0, 0, 2, 0x22, 0x7d,
                                           0xe0, 0x87, 0x78]))

    w_method = create_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])
    result = execute_frame(w_method)
    assert result.as_string() == 'b1'

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
    push = find_symbol(ContextPart, "push:")
    sender = find_symbol(ContextPart, "sender")
    jump = find_symbol(ContextPart, "jump")
    
    bytes = reduce(operator.add, map(chr, [0x21, 0x82, 0xc0, # Set a
                                           0x8f, 0x00, 0x00, 0x0b, # Push block
                                                0x89, 0xd3, # Send sender
                                                0x77, 0xe2, # Send push
                                                0x87, 0x89, 0xd3, 0xd4, # Send jump
                                                0x87, 0x25, 0x7d, # Block rest (not executed)
                                           0xc9, 0x82, 0xc0, 0x40, 0x7c])) # Send value and return
    
    Association = space.classtable["w_Point"] # Wrong class, doesn't matter.
    assoc = model.W_PointersObject(space, Association, 2)
    assoc.store(space, 0, w('a'))
    assoc.store(space, 1, w(3))
    w_method = create_method(bytes, [assoc, w(5), push, sender, jump, w(10)])
    result = execute_frame(w_method)
    assert isinstance(result, model.W_SmallInteger)
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
    push = find_symbol(ContextPart, "push:")
    jump = find_symbol(ContextPart, "jump")
    
    bytes = reduce(operator.add, map(chr, [0x21, 0x82, 0xc0, # Set a
                                           0x89, 0x82, 0xc2, # Set outer
                                           0x8f, 0x00, 0x00, 0x15, # Push block
                                                0x8f, 0x00, 0x00, 0x0f, # Push block
                                                    0x8f, 0x00, 0x00, 0x09, # Push block
                                                        0x42, 0x77, 0xe3, # Push 2
                                                        0x87, 0x42, 0xd4, # Send jump
                                                        0x87, 0x25, 0x7d, # Block rest (not executed)
                                                    0xc9, 0x7d, # Send value and return
                                                0xc9, 0x7d, # Send value and return
                                           0xc9, 0x82, 0xc0, 0x40, 0x7c])) # Send value and return
    
    Association = space.classtable["w_Point"] # Wrong class, doesn't matter.
    assoc = model.W_PointersObject(space, Association, 2)
    assoc.store(space, 0, w('a'))
    assoc.store(space, 1, space.w_nil)
    assoc2 = model.W_PointersObject(space, Association, 2)
    assoc2.store(space, 0, w('outer'))
    assoc2.store(space, 1, space.w_nil)
    w_method = create_method(bytes, [assoc, w(5), assoc2, push, jump, w(10)])
    result = execute_frame(w_method)
    assert isinstance(result, model.W_SmallInteger)
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
    contextOnDo = find_symbol(ContextPartClass, "contextOn:do:")
    
    bytes = reduce(operator.add, map(chr, [
        0x42, 0x43, # Push the classes
        0x8f, 0x00, 0x00, 0x02, # Push block,
            0x24, 0x7d, # in the block
        0xf1, 0x81, 0xc0, 0x7c # Send contextOn:do:
    ]))
    
    Association = space.classtable["w_Point"] # Wrong class, doesn't matter.
    ctxAssoc = model.W_PointersObject(space, Association, 2)
    ctxAssoc.store(space, 0, w('ctx'))
    ctxAssoc.store(space, 1, space.w_nil)
    contextPartAssoc = model.W_PointersObject(space, Association, 2)
    contextPartAssoc.store(space, 0, w('ContextPart'))
    contextPartAssoc.store(space, 1, ContextPart)
    errorAssoc = model.W_PointersObject(space, Association, 2)
    errorAssoc.store(space, 0, w('Point'))
    errorAssoc.store(space, 1, Association)
    w_method = create_method(bytes, [ctxAssoc, contextOnDo, contextPartAssoc, errorAssoc, w('nothing')])
    
    interp.trace = True
    
    result = execute_frame(w_method)
    assert isinstance(result, model.W_PointersObject)
    s = result.as_context_get_shadow(space)
    assert s.w_method().lookup_selector == "on:do:"
    assert s.w_method().primitive() == 199
    assert s.s_sender() == None
    