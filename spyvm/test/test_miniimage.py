import py, math
from spyvm import model, constants, storage_contexts, wrapper, primitives, interpreter, error
from .util import read_image, open_reader, copy_to_module, cleanup_module, TestInterpreter, slow_test, very_slow_test

pytestmark = slow_test

def setup_module():
    space, interp, image, reader = read_image("mini.image")
    w = space.w
    def perform_wrapper(receiver, selector, *args):
        w_selector = None if isinstance(selector, str) else selector
        return interp.perform(receiver, selector, w_selector, list(args))
    perform = perform_wrapper
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def runningSomethingImage(cached=True):
    # This image has been created by executing the followin entire line in a workspace:
    # a := Smalltalk snapshotPrimitive. 1+2.
    # This way, the first few operations when opening this image are predetermined.
    space, interp, _, _ = read_image('mini-running-something.image', cached=cached)
    return space, interp

def runningExitImage(cached=True):
    # This image has been created by executing the followin entire line in a workspace:
    # Smalltalk snapshotPrimitive. Smalltalk snapshot: false andQuit: true.
    # After starting, the image quits immediately. This allows testing the full image execution.
    space, interp, _, _ = read_image('mini-running-exit.image', cached=cached)
    return space, interp

def get_float_class():
    return image.special(constants.SO_FLOAT_CLASS)

# ------ tests ------------------------------------------

def test_read_header():
    assert reader.endofmemory == 726592
    assert reader.oldbaseaddress == -1221464064
    assert reader.specialobjectspointer == -1221336216

def test_read_all_header():
    reader = open_reader(space, "mini.image")
    reader.read_header()
    next = reader.stream.peek()
    assert next != 0 #expects object header, which must not be 0x00000000

def _test_all_pointers_are_valid(reader):
    for each in reader.chunks.itervalues():
        if each.format < 5:
            for pointer in each.data:
                if (pointer & 1) != 1:
                    assert pointer in reader.chunks

def test_all_pointers_are_valid():
    _test_all_pointers_are_valid(reader)

def test_there_are_31_compact_classes():
    assert len(reader.compactclasses) == 31

def test_float_class_size():
    w_float_class = get_float_class()
    assert w_float_class.size() == 9

def test_float_class_name():
    w_float_class = get_float_class()
    w_float_class_name = w_float_class.fetch(space, 6)
    assert isinstance(w_float_class_name, model.W_BytesObject)
    assert w_float_class_name.bytes == list("Float")

# TODO - many of these test would belong in test_model.py

def test_str_float():
    assert str(space.wrap_float(3.0)) == "a Float(3.000000)"

def test_str_string():
    assert str(space.wrap_string('hello')) == "a String('hello')"

def test_str_float():
    assert str(space.wrap_float(3.0)) == "a Float(3.000000)"

def test_str_class_object():
    w_float_class = get_float_class()
    w_float_class.as_class_get_shadow(space)
    assert str(w_float_class) == "Float"

    w_float_class.class_shadow(space)
    assert str(w_float_class.getclass(space)) == "Float class"

    w_float_class.getclass(space).class_shadow(space)
    assert str(w_float_class.getclass(space).getclass(space)) == "Metaclass"

    w_float_class.getclass(space).getclass(space).class_shadow(space)
    assert str(w_float_class.getclass(space).getclass(space).getclass(space)) == "Metaclass class"

def test_nil_true_false():
    w = image.special(constants.SO_NIL)
    w.class_shadow(space)
    assert str(w) == "a UndefinedObject"
    w = image.special(constants.SO_FALSE)
    w.class_shadow(space)
    assert str(w) == "a False"
    w = image.special(constants.SO_TRUE)
    w.class_shadow(space)
    assert str(w) == "a True"

def test_scheduler():
    w = image.special(constants.SO_SCHEDULERASSOCIATIONPOINTER)
    w0 = w.fetch(space, 0)
    assert str(w0) == "a Symbol('Processor')"
    w0 = w.fetch(space, 1)
    w0.class_shadow(space)
    assert str(w0) == "a ProcessorScheduler"

def test_special_classes0():
    def test_classname(so_index, expected_name):
        obj = image.special(so_index)
        obj.as_class_get_shadow(space)
        assert str(obj) == expected_name
    # w = image.special(constants.SO_BITMAP_CLASS)
    # assert str(w) == "Bitmap"
    test_classname(constants.SO_SMALLINTEGER_CLASS, "SmallInteger")
    test_classname(constants.SO_ARRAY_CLASS, "Array")
    test_classname(constants.SO_FLOAT_CLASS, "Float")
    test_classname(constants.SO_METHODCONTEXT_CLASS, "MethodContext")
    test_classname(constants.SO_BLOCKCONTEXT_CLASS, "BlockContext")
    test_classname(constants.SO_POINT_CLASS, "Point")
    test_classname(constants.SO_LARGEPOSITIVEINTEGER_CLASS, "LargePositiveInteger")
    test_classname(constants.SO_MESSAGE_CLASS, "Message")

    # to be continued

    """SO_SMALLTALK = 8
     SO_DISPLAY_CLASS = 14
    SO_MESSAGE_CLASS = 15
    SO_COMPILEDMETHOD_CLASS = 16
    SO_LOW_SPACE_SEMAPHORE = 17
    SO_SEMAPHORE_CLASS = 18
    SO_CHARACTER_CLASS = 19"""

def test_name_of_shadow_of_specials():
    w_doesnot = image.special(constants.SO_DOES_NOT_UNDERSTAND)
    assert repr(w_doesnot.class_shadow(space)) == "<ClassShadow Symbol>"
    assert repr(space.w_nil.class_shadow(space)) == "<ClassShadow UndefinedObject>"
    assert repr(space.w_minus_one.class_shadow(space)) == "<ClassShadow SmallInteger>"
    assert repr(space.w_zero.class_shadow(space)) == "<ClassShadow SmallInteger>"
    assert repr(space.w_one.class_shadow(space)) == "<ClassShadow SmallInteger>"
    assert repr(space.w_two.class_shadow(space)) == "<ClassShadow SmallInteger>"
    assert repr(space.w_true.class_shadow(space)) == "<ClassShadow True>"
    assert repr(space.w_false.class_shadow(space)) == "<ClassShadow False>"

def test_special_objects0():
    w = image.special(constants.SO_DOES_NOT_UNDERSTAND)
    assert str(w) == "a Symbol('doesNotUnderstand:')"
    assert str(w.getclass(space)) == "Symbol" # for some strange reason not a symbol


    """
    SO_DOES_NOT_UNDERSTAND = 20
    SO_CANNOT_RETURN = 21
    SO_PROCESS_SIGNALIGN_LOW_SPACE = 22 # the process that triggered the low space semaphore. mostly nil
    SO_SPECIAL_SELECTORS_ARRAY = 23
    SO_CHARACTER_TABLE_ARRAY = 24
    SO_MUST_BE_BOOLEAN = 25
    SO_BYTEARRAY_CLASS = 26
    SO_PROCESS_CLASS = 27
    SO_COMPACT_CLASSES_ARRAY = 28
    SO_TIMER_SEMAPHORE = 29
    SO_USER_INTERRUPT_SEMAPHORE = 30
    SO_FLOAT_ZERO = 31
    SO_LARGEPOSITIVEINTEGER_ZERO = 32
    SO_A_POINT = 33
    SO_CANNOT_INTERPRET = 34
    SO_A_METHODCONTEXT = 35 # deprecated in closure images
    SO_BLOCKCLOSURE_CLASS = 36
    SO_A_BLOCKCONTEXT = 37 # deprecated in closure images
    SO_EXTERNAL_OBJECTS_ARRAY = 38
    SO_PSEUDOCONTEXT_CLASS = 39
    SO_TRANSLATEDMETHOD_CLASS = 40
    SO_FINALIZATION_SEMPAHORE = 41
    SO_LARGENEGATIVEINTEGER_CLASS = 42
    """

def _test_lookup_abs_in_integer(interp):
    w_abs = interp.perform(interp.space.w("abs"), "asSymbol")
    for value in [10, -3, 0]:
        w_object = model.W_SmallInteger(value)
        w_res = interp.perform(w_object, w_selector=w_abs)
        assert w_res.value == abs(value)

def test_lookup_abs_in_integer():
    _test_lookup_abs_in_integer(interp)

def test_map_mirrors_to_classtable():
    w_compiledmethod_class = image.special(constants.SO_COMPILEDMETHOD_CLASS)
    assert w_compiledmethod_class.is_same_object(space.w_CompiledMethod)
    w_nil = image.special(constants.SO_NIL)
    assert w_nil.is_same_object(space.w_nil)
    w_true = image.special(constants.SO_TRUE)
    assert w_true.is_same_object(space.w_true)
    w_false = image.special(constants.SO_FALSE)
    assert w_false.is_same_object(space.w_false)

@very_slow_test
def test_runimage_and_quit():
    from targetimageloadingsmalltalk import active_context, execute_context
    space, interp = runningExitImage(cached=False)
    frame = active_context(space)
    try:
        execute_context(interp, frame)
    except error.Exit, e:
        assert e.msg == "Quit-Primitive called"

def test_step_forged_image():
    ap = wrapper.ProcessWrapper(space, wrapper.scheduler(space).active_process())
    s_ctx = ap.suspended_context().as_context_get_shadow(space)
    assert isinstance(s_ctx, storage_contexts.ContextPartShadow)
    assert s_ctx.top().is_same_object(space.w_true)

def test_create_new_symbol():
    w_result = perform(w("someString"), "asSymbol")
    assert w_result is not None
    assert w_result.as_string() == "someString"

def test_create_new_symbol_new_with_arg0():
    w_dnu = image.special(constants.SO_DOES_NOT_UNDERSTAND)
    w_Symbol = w_dnu.getclass(space)
    w_res = perform(w_Symbol, "new:", w(0))
    assert w_res.getclass(space).is_same_object(w_Symbol)
    assert isinstance(w_res, model.W_BytesObject)
    assert w_res.size() == 0

def test_pi_as_w_float():
    w_result = perform(interp.space.w_Float, "pi")
    assert w_result is not None
    assert isinstance(w_result, model.W_Float)
    assert w_result.value == math.pi

def test_new_float_as_w_float():
    w_result = perform(interp.space.w_Float, "new")
    assert w_result is not None
    assert isinstance(w_result, model.W_Float)

def test_existing_large_positive_integer_as_W_LargePositiveInteger1Word():
    w_result = perform(interp.space.w_Float, "pi")
    assert w_result is not None
    assert isinstance(w_result, model.W_Float)
    assert w_result.value == math.pi

def test_large_positive_integer_operations():
    w_result = perform(interp.space.w_SmallInteger, "maxVal")
    w_result = perform(w_result, "+", interp.space.wrap_int(2 * interp.space.unwrap_int(w_result)))
    assert w_result is not None
    assert isinstance(w_result, model.W_LargePositiveInteger1Word)

    w_result = perform(interp.space.w_SmallInteger, "maxVal")
    w_result = perform(w_result, "*", w_result)
    assert w_result is not None
    assert isinstance(w_result, model.W_BytesObject)

def test_doesNotUnderstand():
    w_dnu = interp.space.objtable["w_doesNotUnderstand"]
    assert isinstance(w_dnu, model.W_BytesObject)
    assert w_dnu.as_string() == "doesNotUnderstand:"

def test_mustBeBoolean():
    w_mbb = interp.space.objtable["w_mustBeBoolean"]
    assert isinstance(w_mbb, model.W_BytesObject)
    assert w_mbb.as_string() == "mustBeBoolean"

def test_Message():
    w_message_cls = interp.space.w_Message
    assert w_message_cls is interp.space.classtable["w_Message"]
    assert isinstance(w_message_cls, model.W_PointersObject)
    s_message_cls = w_message_cls.as_class_get_shadow(interp.space)
    assert s_message_cls.getname() == "Message"
    w_message = s_message_cls.new()
    assert isinstance(w_message, model.W_PointersObject)

def test_primitive_perform_with_args():
    from spyvm.test.test_primitives import _prim
    w_o = space.wrap_list([1, 2, 3])
    w_methoddict = w_o.class_shadow(space).s_superclass().s_superclass().w_methoddict()
    w_methoddict.as_methoddict_get_shadow(space).sync_method_cache()
    selectors_w = w_methoddict.shadow.methoddict.keys()
    w_sel = None
    for sel in selectors_w:
        if sel.as_string() == 'size':
            w_sel = sel
    size = _prim(space, primitives.PERFORM_WITH_ARGS, [w_o, w_sel, []])
    assert size.value == 3

def test_step_run_something():
    space, interp = runningSomethingImage(cached=False)
    ap = wrapper.ProcessWrapper(space, wrapper.scheduler(space).active_process())
    w_ctx = ap.suspended_context()
    s_ctx = w_ctx.as_context_get_shadow(space)
    ap.store_suspended_context(space.w_nil)

    assert isinstance(s_ctx, storage_contexts.ContextPartShadow)
    assert s_ctx.top().is_same_object(space.w_true)
    interp.step(s_ctx)
    interp.step(s_ctx)
    assert s_ctx.top().value == 1
    interp.step(s_ctx)
    assert s_ctx.top().value == 2
    interp.step(s_ctx)
    assert s_ctx.top().value == 3

def test_run_doesNotUnderstand():
    space, interp = runningSomethingImage()
    w_result = interp.perform(interp.space.wrap_int(0), "runningADNU")
    assert isinstance(w_result, model.W_BytesObject)
    assert w_result.as_string() == "foobarThis:doesNotExist:('pypy' 'heya' )"

def test_run_mustBeBoolean():
    space, interp = runningSomethingImage()
    w_result = interp.perform(interp.space.wrap_int(0), "runningMustBeBoolean")
    assert isinstance(w_result, model.W_BytesObject)
    assert w_result.as_string() == "mustBeBoolean has been called"
