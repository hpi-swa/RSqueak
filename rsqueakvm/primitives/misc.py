import os

from rsqueakvm import constants, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.primitives import expose_primitive, pos_32bit_int
from rsqueakvm.primitives.constants import *
from rsqueakvm.primitives.storage import get_instances_array

from rpython.rlib import jit


# ___________________________________________________________________________
# Miscellaneous Primitives (120-127)

@expose_primitive(IMAGE_NAME)
def func(interp, s_frame, argument_count):
    from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX
    if argument_count == 0:
        s_frame.pop()
        return interp.space.wrap_string(interp.space.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX))
    elif argument_count == 1:
        w_arg = s_frame.pop()
        assert isinstance(w_arg, W_BytesObject)
        arg = interp.space.unwrap_string(w_arg)
        interp.space.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, arg)
        interp.space.display().set_title(arg)
        return s_frame.pop()
    raise PrimitiveFailedError

@expose_primitive(LOW_SPACE_SEMAPHORE, unwrap_spec=[object, object])
def func(interp, s_frame, w_receiver, i):
    # dont know when the space runs out
    return w_receiver

@expose_primitive(SIGNAL_AT_BYTES_LEFT, unwrap_spec=[object, int])
def func(interp, s_frame, w_receiver, i):
    # dont know when the space runs out
    return w_receiver

@expose_primitive(DEFER_UPDATES, unwrap_spec=[object, bool])
def func(interp, s_frame, w_receiver, flag):
    sdldisplay = interp.space.display()
    sdldisplay.defer_updates(flag)
    return w_receiver

@expose_primitive(DRAW_RECTANGLE, unwrap_spec=[object, int, int, int, int])
def func(interp, s_frame, w_rcvr, left, right, top, bottom):
    if not interp.space.objtable['w_display'].is_same_object(w_rcvr):
        return interp.space.w_nil
    if not ((left <= right) and (top <= bottom)):
        return interp.space.w_nil
    form = wrapper.FormWrapper(interp.space, w_rcvr)
    form.get_display_bitmap().force_rectange_to_screen(left, right, top, bottom)
    interp.space.display().flip(force=True)
    return w_rcvr


# ___________________________________________________________________________
# Squeak Miscellaneous Primitives (128-134)

@expose_primitive(BECOME, unwrap_spec=[object, object])
def func(interp, s_frame, w_rcvr, w_new):
    if w_rcvr.size() != w_new.size():
        raise PrimitiveFailedError
    w_lefts = []
    w_rights = []
    for i in range(w_rcvr.size()):
        w_left = w_rcvr.at0(interp.space, i)
        w_right = w_new.at0(interp.space, i)
        if w_left.become(w_right):
            w_lefts.append(w_left)
            w_rights.append(w_right)
        else:
            for i in range(len(w_lefts)):
                w_lefts[i].become(w_rights[i])
            raise PrimitiveFailedError()
    return w_rcvr

@expose_primitive(SPECIAL_OBJECTS_ARRAY, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return interp.image.special_objects

def fake_bytes_left(interp):
    from rsqueakvm.util.platform_calls import get_memory_usage
    usage = get_memory_usage()
    if usage < 0:
        # there was an error getting the result
        return interp.space.wrap_int(2**29)
    else:
        return interp.space.wrap_int(constants.MAXINT - usage)

@expose_primitive(INC_GC, unwrap_spec=[object])
@expose_primitive(FULL_GC, unwrap_spec=[object])
@jit.dont_look_inside
def func(interp, s_frame, w_rcvr):
    # Squeak pops the arg and ignores it ... go figure
    from rpython.rlib import rgc
    rgc.collect()
    return fake_bytes_left(interp)

@expose_primitive(SET_INTERRUPT_KEY, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, encoded_key):
    interp.space.display().set_interrupt_key(interp.space, encoded_key)
    return w_rcvr

@expose_primitive(INTERRUPT_SEMAPHORE, unwrap_spec=[object, object])
def func(interp, s_frame, w_rcvr, w_semaphore):
    if w_semaphore.getclass(interp.space).is_same_object(interp.space.w_Semaphore):
        interp.space.objtable['w_interrupt_semaphore'] = w_semaphore
    else:
        interp.space.objtable['w_interrupt_semaphore'] = interp.space.w_nil
    return w_rcvr


#____________________________________________________________________________
# Misc Primitives (138 - 149)

@expose_primitive(SOME_OBJECT, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    match_w = get_instances_array(interp, s_frame, some_instance=True)
    try:
        return match_w[0]
    except IndexError:
        raise PrimitiveFailedError()

def next_object(space, list_of_objects, w_obj):
    retval = None
    try:
        idx = list_of_objects.index(w_obj)
    except ValueError:
        idx = -1
    try:
        retval = list_of_objects[idx + 1]
    except IndexError:
        return space.wrap_int(0)
    return retval

@expose_primitive(NEXT_OBJECT, unwrap_spec=[object])
def func(interp, s_frame, w_obj):
    # This primitive is used to iterate through all objects:
    # it returns the "next" instance after w_obj.
    return next_object(interp.space, get_instances_array(interp, s_frame), w_obj)

@expose_primitive(ALL_INSTANCES, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    match_w = get_instances_array(interp, s_frame, w_class=w_class, store=False)
    return interp.space.wrap_list(match_w)

@expose_primitive(ALL_OBJECTS, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    match_w = get_instances_array(interp, s_frame, w_class=None, store=False)
    return interp.space.wrap_list(match_w)

@expose_primitive(BEEP, unwrap_spec=[object])
def func(interp, s_frame, w_receiver):
    return w_receiver

@expose_primitive(CLIPBOARD_TEXT)
def func(interp, s_frame, argument_count):
    if argument_count == 0:
        if interp.space.display().has_clipboard_text():
            return interp.space.wrap_string(interp.space.display().get_clipboard_text())
        else:
            return interp.space.wrap_string("")
    elif argument_count == 1:
        w_arg = s_frame.pop()
        interp.space.display().set_clipboard_text(interp.space.unwrap_string(w_arg))

@expose_primitive(VM_PATH, unwrap_spec=[object])
def func(interp, s_frame, w_receiver):
    return interp.space.wrap_string("%s%s" % (interp.space.executable_path(), os.path.sep))

@expose_primitive(FILL, unwrap_spec=[object, pos_32bit_int])
def func(interp, s_frame, w_arg, new_value):
    space = interp.space
    if isinstance(w_arg, W_BytesObject):
        if new_value > 255:
            raise PrimitiveFailedError
        for i in xrange(w_arg.size()):
            w_arg.setchar(i, chr(new_value))
    elif isinstance(w_arg, W_WordsObject) or isinstance(w_arg, W_DisplayBitmap):
        for i in xrange(w_arg.size()):
            w_arg.setword(i, new_value)
    else:
        raise PrimitiveFailedError
    return w_arg

@expose_primitive(CLONE, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    return w_arg.clone(interp.space)

@expose_primitive(SYSTEM_ATTRIBUTE, unwrap_spec=[object, int])
def func(interp, s_frame, w_receiver, attr_id):
    try:
        return interp.space.wrap_string("%s" % interp.space.get_system_attribute(attr_id))
    except KeyError:
        return interp.space.w_nil
