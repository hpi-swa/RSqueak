from spyvm import squeakimage, model, constants, interpreter, shadow, objspace
from .util import read_image, find_symbol_in_methoddict_of, copy_to_module, cleanup_module

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

def test_ensure():
    #ensure
    #    [^'b1'] ensure: [^'b2']
    import operator
    bytes = reduce(operator.add, map(chr, [0x8F, 0, 0, 2, 0x21, 0x7c,
                                           0x8F, 0, 0, 2, 0x22, 0x7c,
                                           0xe0, 0x87, 0x78]))

    s_class = space.w_BlockClosure.as_class_get_shadow(space)
    ensure_ = find_symbol_in_methoddict_of('ensure:', s_class)
    assert ensure_ is not None, 'Using image without #ensure:-method.'

    w_method = create_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])

    # create a frame for our newly crafted method with a valid sender (to avoid raising returnFromTop to early)
    s_initial_frame = create_method(chr(0x7c)).create_frame(space, w(0), [])
    s_frame = w_method.create_frame(space, w(0))
    s_frame.store_s_sender(s_initial_frame, raise_error=False)
    
    try:
        interp.loop(s_frame.w_self())
    except interpreter.ReturnFromTopLevel, e:
        assert e.object.as_string() == 'b2'
    except interpreter.StackOverflow, e:
        assert False

def test_ensure_save_original_nlr():
    #ensure
    #    [^'b1'] ensure: ['b2']
    import operator
    bytes = reduce(operator.add, map(chr, [0x8F, 0, 0, 2, 0x21, 0x7c,
                                           0x8F, 0, 0, 2, 0x22, 0x7d,
                                           0xe0, 0x87, 0x78]))

    s_class = space.w_BlockClosure.as_class_get_shadow(space)
    ensure_ = find_symbol_in_methoddict_of('ensure:', s_class)
    assert ensure_ is not None, 'Using image without #ensure:-method.'

    w_method = create_method(bytes, [ensure_, w('b1'), w('b2'),
                                            w('ensure'), space.w_BlockClosure])

    # create a frame for our newly crafted method with a valid sender (to avoid raising returnFromTop to early)
    s_initial_frame = create_method(chr(0x7c)).create_frame(space, w(0))
    s_frame = w_method.create_frame(space, w(0))
    s_frame.store_s_sender(s_initial_frame, raise_error=False)
    
    try:
        interp.loop(s_frame.w_self())
    except interpreter.ReturnFromTopLevel, e:
        assert e.object.as_string() == 'b1'
    except interpreter.StackOverflow, e:
        assert False
