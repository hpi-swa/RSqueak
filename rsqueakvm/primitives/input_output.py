import os

from rsqueakvm import display, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_WordsObject
from rsqueakvm.primitives import expose_primitive, assert_class, index1_0
from rsqueakvm.primitives.constants import *

from rpython.rlib import jit


# ___________________________________________________________________________
# File primitives (150-169)
# (XXX they are obsolete in Squeak and done with a plugin)

@expose_primitive(FILE_CLOSE, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, fd):
    try:
        os.close(fd)
    except OSError:
        raise PrimitiveFailedError()
    return w_rcvr

@expose_primitive(FILE_OPEN, unwrap_spec=[object, str, object])
def func(interp, s_frame, w_rcvr, filename, w_writeable_flag):
    if w_writeable_flag.is_same_object(interp.space.w_true):
        mode = os.O_RDWR | os.O_CREAT | os.O_TRUNC
    else:
        mode = os.O_RDONLY
    try:
        fd = os.open(filename, mode, 0666)
    except OSError:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(fd)

@expose_primitive(FILE_WRITE, unwrap_spec=[object, int, str, int, int])
def func(interp, s_frame, w_rcvr, fd, src, start, count):
    start = start - 1
    end = start + count
    if end < 0 or start < 0:
        raise PrimitiveFailedError()
    try:
        os.write(fd, src[start:end])
    except OSError:
        raise PrimitiveFailedError()
    return w_rcvr

@expose_primitive(DIRECTORY_DELIMITOR, unwrap_spec=[object])
def func(interp, s_frame, _):
    return interp.space.wrap_char(os.path.sep)

# ___________________________________________________________________________
# I/O Primitives

@expose_primitive(MOUSE_POINT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    x, y = interp.space.display().mouse_point()
    w_point = W_PointersObject(interp.space, interp.space.w_Point, 2)
    w_point.store(interp.space, 0, interp.space.wrap_int(x))
    w_point.store(interp.space, 1, interp.space.wrap_int(y))
    return w_point

@expose_primitive(GET_NEXT_EVENT, unwrap_spec=[object, object])
@jit.unroll_safe
@jit.look_inside
def func(interp, s_frame, w_rcvr, w_into):
    if not interp.evented:
        raise PrimitiveFailedError()
    try:
        ary = interp.space.display().get_next_event(time=interp.event_time_now())
    except display.SqueakInterrupt, e:
        w_interrupt_sema = interp.space.objtable['w_interrupt_semaphore']
        if w_interrupt_sema is not interp.space.w_nil:
            assert_class(interp, w_interrupt_sema, interp.space.w_Semaphore)
            wrapper.SemaphoreWrapper(interp.space, w_interrupt_sema).signal(s_frame)
        else:
            raise e
    else:
        for i in range(8):
            w_into.store(interp.space, i, interp.space.wrap_int(ary[i]))
        # XXX - hack
        if (ary[0] == display.WindowEventMetricChange and
                ary[4] > 0 and ary[5] > 0):
            if interp.image:
                interp.image.lastWindowSize = ((ary[4] & 0xffff) << 16) | (ary[5] & 0xffff)
    return w_rcvr

@expose_primitive(BITBLT_COPY_BITS, clean_stack=False, no_result=True,
                  compiled_method=True)
def func(interp, s_frame, argcount, w_method):
    w_name = interp.space.wrap_string("primitiveCopyBits")
    signature = ("BitBltPlugin", "primitiveCopyBits")
    from rsqueakvm.plugins.simulation import SimulationPlugin
    return SimulationPlugin.simulate(w_name, signature, interp, s_frame, argcount, w_method)

@expose_primitive(SNAPSHOT, clean_stack=False, no_result=True)
def func(interp, s_frame, argcount):
    s_frame.pop_n(argcount)
    s_frame.push(interp.space.w_true)
    # leaving true on the frame as return value for resuming image
    from rsqueakvm.squeakimage import SpurImageWriter
    from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX
    filename = interp.space.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX)
    SpurImageWriter(interp, filename).trace_image(s_frame)
    s_frame.pop()
    s_frame.push(interp.space.w_false)  # the non-resuming image gets false

@expose_primitive(BE_CURSOR)
def func(interp, s_frame, argcount):
    if not (0 <= argcount <= 1):
        raise PrimitiveFailedError()
    w_rcvr = s_frame.peek(argcount)

    if interp.space.headless.is_set():
        # we don't do any cursoration if we're headless
        return w_rcvr

    mask_words = None
    if argcount == 1:
        w_mask = s_frame.peek(0)
        if isinstance(w_mask, W_WordsObject):
            mask_words = w_mask.words
        elif isinstance(w_mask, W_PointersObject):
            # mask is a form object
            w_contents = w_mask.fetch(interp.space, 0)
            if isinstance(w_contents, W_WordsObject):
                mask_words = w_contents.words
            else:
                raise PrimitiveFailedError
        else:
            raise PrimitiveFailedError()
    w_bitmap = w_rcvr.fetch(interp.space, 0)
    if not isinstance(w_bitmap, W_WordsObject):
        raise PrimitiveFailedError()
    width = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 1))
    height = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 2))
    depth = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 3))
    hotpt = wrapper.PointWrapper(interp.space, w_rcvr.fetch(interp.space, 4))
    offx = hotpt.x()
    offy = hotpt.y()
    if not (width == 16 and height == 16 and depth == 1 and
            offx >= -16 and offy >= -16 and
            offx <= 0 and offy <= 0):
        raise PrimitiveFailedError
    offx = -offx
    offy = -offy

    if display.SDLCursor.set(w_bitmap.words, width, height, offx, offy,
                             mask_words=mask_words):
        interp.space.objtable['w_cursor'] = w_rcvr
    # Don't fail if the Cursor could not be set.
    # It is surely annoying but no reason to not continue.
    s_frame.pop_n(argcount + 1)
    return w_rcvr

@expose_primitive(BE_DISPLAY, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        s_frame.exitFromHeadlessExecution()
    if not isinstance(w_rcvr, W_PointersObject) or w_rcvr.size() < 4:
        raise PrimitiveFailedError

    old_display = interp.space.objtable['w_display']
    if isinstance(old_display, W_DisplayBitmap):
        old_display.relinquish_display()
    interp.space.objtable['w_display'] = w_rcvr

    form = wrapper.FormWrapper(interp.space, w_rcvr)
    form.take_over_display()
    w_display_bitmap = form.get_display_bitmap()
    w_display_bitmap.take_over_display()
    w_display_bitmap.flush_to_screen()

    if interp.image:
        interp.image.lastWindowSize = (form.width() << 16) + form.height()
    return w_rcvr

@expose_primitive(REPLACE_FROM_TO, unwrap_spec=[object, index1_0, index1_0, object, index1_0])
@jit.look_inside_iff(lambda interp, s_frame, w_rcvr, start, stop, w_replacement, repStart: jit.isconstant(stop) and jit.isconstant(start) and (stop - start < 13)) # heuristic
def func(interp, s_frame, w_rcvr, start, stop, w_replacement, repStart):
    """replaceFrom: start to: stop with: replacement startingAt: repStart
    Primitive. This destructively replaces elements from start to stop in the
    receiver starting at index, repStart, in the collection, replacement. Answer
    the receiver. Range checks are performed in the primitive only. Essential
    for Pharo Candle Symbols.
    | index repOff |
    repOff := repStart - start.
    index := start - 1.
    [(index := index + 1) <= stop]
        whileTrue: [self at: index put: (replacement at: repOff + index)]"""
    if (start < 0 or start - 1 > stop or repStart < 0):
        raise PrimitiveFailedError()
    if (w_rcvr.varsize() <= stop or w_replacement.varsize() <= repStart + (stop - start)):
        raise PrimitiveFailedError()
    repOff = repStart - start
    for i0 in range(start, stop + 1):
        w_rcvr.atput0(interp.space, i0, w_replacement.at0(interp.space, repOff + i0))
    return w_rcvr

@expose_primitive(SCREEN_SIZE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    w_res = interp.space.w_Point.as_class_get_shadow(interp.space).new(2)
    point = wrapper.PointWrapper(interp.space, w_res)
    display = interp.space.display()
    if display.width == 0:
        # We need to have the indirection via interp.image, because when the image
        # is saved, the display form size is always reduced to 240@120.
        if not interp.image:
            raise PrimitiveFailedError
        display.width = (interp.image.lastWindowSize >> 16) & 0xffff
        display.height = interp.image.lastWindowSize & 0xffff
    point.store_x(display.width)
    point.store_y(display.height)
    return w_res

@expose_primitive(MOUSE_BUTTONS, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    btn = interp.space.display().mouse_button()
    return interp.space.wrap_int(btn)

@expose_primitive(KBD_NEXT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    code = interp.space.display().next_keycode()
    if code & 0xFF == 0:
        return interp.space.w_nil
    else:
        return interp.space.wrap_int(code)

@expose_primitive(KBD_PEEK, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    code = interp.space.display().peek_keycode()
    # TODO: how do old images handle CmdDot? See INTERRUPT_SEMAPHORE?
    if code & 0xFF == 0:
        return interp.space.w_nil
    else:
        return interp.space.wrap_int(code)
