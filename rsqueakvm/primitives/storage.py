from rsqueakvm import constants, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.block_closure import W_BlockClosure
from rsqueakvm.model.compiled_methods import (W_CompiledMethod,
                                              W_PreSpurCompiledMethod,
                                              W_SpurCompiledMethod)
from rsqueakvm.model.numeric import W_SmallInteger
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.primitives import (expose_primitive, expose_also_as,
                                  assert_pointers, index1_0)
from rsqueakvm.primitives.constants import *

from rpython.rlib import jit, objectmodel


# ___________________________________________________________________________
# Storage Management Primitives

# these primitives are also called as functions from elsewhere
# hence they have proper names

@expose_primitive(SLOT_AT, unwrap_spec=[object, index1_0])
def primitive_fetch(interp, s_frame, w_rcvr, n0):
    try:
        return w_rcvr.fetch(interp.space, n0)
    except IndexError:
        raise PrimitiveFailedError

@expose_primitive(SLOT_AT_PUT, unwrap_spec=[object, index1_0, object])
def primitive_store(interp, s_frame, w_rcvr, n0, w_value):
    try:
        w_rcvr.store(interp.space, n0, w_value)
        return w_value
    except IndexError:
        raise PrimitiveFailedError

@expose_primitive(OBJECT_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_rcvr, n0):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    return w_rcvr.literalat0(interp.space, n0)

@expose_primitive(OBJECT_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_rcvr, n0, w_value):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    w_rcvr.literalatput0(interp.space, n0, w_value)
    return w_value

@expose_primitive(NEW, unwrap_spec=[object])
def func(interp, s_frame, w_cls):
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    if s_class.isvariable():
        raise PrimitiveFailedError()
    return s_class.new()

@expose_primitive(NEW_WITH_ARG, unwrap_spec=[object, int])
def func(interp, s_frame, w_cls, size):
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    if not s_class.isvariable() and size != 0:
        raise PrimitiveFailedError()
    if size < 0:
        raise PrimitiveFailedError()
    try:
        return s_class.new(size)
    except MemoryError:
        raise PrimitiveFailedError

@expose_primitive(ARRAY_BECOME_ONE_WAY, unwrap_spec=[object, object])
def func(interp, s_frame, w_from, w_to):
    if not objectmodel.we_are_translated():
        if hasattr(interp, "shell_execute"):
            return w_from
    from_w = interp.space.unwrap_array(w_from)
    to_w = interp.space.unwrap_array(w_to)
    space = interp.space
    if len(from_w) != len(to_w):
        raise PrimitiveFailedError

    # TODO: make this fast (context-switch and stack-rebuilding?)
    s_current = s_frame
    while s_current.has_s_sender():
        s_current.w_self()  # just for the side effect of creating the
                            # ContextPart object
        s_current = s_current.s_sender()

    from rpython.rlib import rgc
    roots = [gcref for gcref in rgc.get_rpy_roots() if gcref]
    pending = roots[:]
    while pending:
        gcref = pending.pop()
        if not rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            w_obj = rgc.try_cast_gcref_to_instance(W_Object, gcref)
            if w_obj is not None and w_obj.has_class():
                w_obj.pointers_become_one_way(space, from_w, to_w)
            pending.extend(rgc.get_rpy_referents(gcref))
    while roots:
        gcref = roots.pop()
        if rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            roots.extend(rgc.get_rpy_referents(gcref))
    return w_from

@expose_primitive(INST_VAR_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_rcvr, n0):
    "Fetches a fixed field from the object, and fails otherwise"
    s_class = w_rcvr.class_shadow(interp.space)
    w_cls = assert_pointers(w_rcvr)
    return primitive_fetch(interp, s_frame, w_rcvr, n0)

@expose_primitive(INST_VAR_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_rcvr, n0, w_value):
    "Stores a value into a fixed field from the object, and fails otherwise"
    s_class = w_rcvr.class_shadow(interp.space)
    w_rcvr = assert_pointers(w_rcvr)
    return primitive_store(interp, s_frame, w_rcvr, n0, w_value)

@expose_primitive(CHARACTER_VALUE)
def func(interp, s_frame, argument_count):
    w_value = s_frame.peek(0)
    s_frame.pop_n(argument_count + 1)
    value = interp.space.unwrap_int(w_value)
    return W_Character(value)

@expose_also_as(IMMEDIATE_IDENTITY_HASH, CLASS_IDENTITY_HASH)
@expose_primitive(AS_OOP, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_SmallInteger):
        raise PrimitiveFailedError()
    return interp.space.wrap_int(w_rcvr.gethash())

_maximum_identity_hash = 2**22 - 1
@expose_primitive(MAX_IDENTITY_HASH, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    return interp.space.wrap_int(_maximum_identity_hash)

@expose_primitive(STORE_STACKP, unwrap_spec=[object, int])
def func(interp, s_frame, w_frame, stackp):
    assert stackp >= 0, "trying to store a negative stackp in STORE_STACKP"
    w_frame = assert_pointers(w_frame)
    w_frame.store(interp.space, constants.CTXPART_STACKP_INDEX,
                  interp.space.wrap_int(stackp))
    return w_frame

def get_instances_array_gc(interp, w_class=None):
    space = interp.space
    from rpython.rlib import rgc

    result_w = []
    roots = [gcref for gcref in rgc.get_rpy_roots() if gcref]
    pending = roots[:]
    while pending:
        gcref = pending.pop()
        if not rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            w_obj = rgc.try_cast_gcref_to_instance(W_Object, gcref)

            if w_obj is not None and w_obj.has_class():
                w_cls = w_obj.getclass(space)
                if w_cls is not None:
                    # XXX: should not return SmallFloat64 on Spur64...
                    if ((not w_cls.is_same_object(space.w_SmallInteger)) and
                        (not (space.is_spur.is_set() and w_cls.is_same_object(space.w_Character))) and
                        (w_class is None or w_cls.is_same_object(w_class))):
                        result_w.append(w_obj)
            pending.extend(rgc.get_rpy_referents(gcref))

    rgc.clear_gcflag_extra(roots)
    rgc.assert_no_more_gcflags()
    return result_w

@jit.dont_look_inside
def get_instances_array_trace(interp, w_class, some_instance=False):
    space = interp.space
    result_w = []
    seen_w = {}
    roots = [interp.image.special_objects]
    pending = roots[:]
    while pending:
        w_obj = pending.pop()
        if w_obj and not seen_w.get(w_obj, False):
            seen_w[w_obj] = True
            if w_obj.has_class():
                w_cls = w_obj.getclass(space)
                if w_cls is not None:
                    # XXX: should not return SmallFloat64 on Spur64...
                    if ((not w_cls.is_same_object(space.w_SmallInteger)) and
                        (not (space.is_spur.is_set() and w_cls.is_same_object(space.w_Character))) and
                        (w_class is None or w_cls.is_same_object(w_class))):
                        if some_instance:
                            return [w_obj]
                        else:
                            result_w.append(w_obj)
            pending.extend(_trace_pointers(interp.space, w_obj))
    return result_w

def _trace_pointers(space, w_obj):
    p_w = [w_obj.getclass(space)]
    if isinstance(w_obj, W_CompiledMethod):
        p_w.extend(w_obj.literals)
    elif isinstance(w_obj, W_PointersObject):
        p_w.extend(w_obj.fetch_all(space))
    elif isinstance(w_obj, W_BlockClosure):
        p_w.extend(w_obj.fetch_all(space))
    return p_w

def get_instances_array(interp, s_frame, w_class=None, store=True,
                        some_instance=False):
    # early return for classes that never had instances. This is true for a
    # bunch of special classes.
    if w_class:
        if interp.space.w_SmallInteger.is_same_object(w_class):
            return []
        if interp.space.is_spur.is_set():
            if interp.space.w_Character.is_same_object(w_class):
                return []
            if interp.image.version.is_64bit:
                if interp.space.w_Float.is_same_object(w_class):
                    return []
        if interp.image.version.is_modern:
            if interp.space.w_BlockContext.is_same_object(w_class):
                return []
    # make sure we also get any objects in the currently active process
    w_active_process = wrapper.scheduler(interp.space).active_process()
    active_process = wrapper.ProcessWrapper(interp.space, w_active_process)
    active_process.store_suspended_context(s_frame.w_self())
    try:
        match_w = s_frame.instances_array(w_class)
        if match_w is None:
            if some_instance and interp.space.is_spur.is_set():
                # on Spur, someInstance really means just one, it's not used to
                # start iterating over all instances
                return get_instances_array_trace(interp, w_class,
                                                 some_instance=True)
            match_w = get_instances_array_trace(interp, w_class)
            if store:
                s_frame.store_instances_array(w_class, match_w)
        return match_w
    finally:
        active_process.store_suspended_context(interp.space.w_nil)


@expose_primitive(SOME_INSTANCE, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    # This primitive returns some instance of the class on the stack.
    # If no class is given, it returns some object.
    if w_class.is_same_object(interp.space.w_SmallInteger):
        raise PrimitiveFailedError()

    match_w = get_instances_array(interp, s_frame, w_class=w_class,
                                  some_instance=True)
    try:
        return match_w[0]
    except IndexError:
        raise PrimitiveFailedError()

def next_instance(space, list_of_objects, w_obj):
    retval = None
    try:
        idx = list_of_objects.index(w_obj)
    except ValueError:
        idx = -1
    try:
        retval = list_of_objects[idx + 1]
    except IndexError:
        raise PrimitiveFailedError()
    # just in case, that one of the objects in the list changes its class
    if retval.getclass(space).is_same_object(w_obj.getclass(space)):
        return retval
    else:
        list_of_objects.pop(idx + 1)
        return next_instance(space, list_of_objects, w_obj)

@expose_primitive(NEXT_INSTANCE, unwrap_spec=[object])
def func(interp, s_frame, w_obj):
    # This primitive is used to iterate through all instances of a class:
    # it returns the "next" instance after w_obj.
    return next_instance(
        interp.space,
        get_instances_array(interp, s_frame,
                            w_class=w_obj.getclass(interp.space)),
        w_obj
    )

@expose_primitive(NEW_METHOD, unwrap_spec=[object, int, int])
def func(interp, s_frame, w_class, bytecount, header):
    # We ignore w_class because W_CompiledMethod subclasses are special
    if interp.space.is_spur.is_set():
        return W_SpurCompiledMethod(interp.space, bytecount, header)
    else:
        return W_PreSpurCompiledMethod(interp.space, bytecount, header)
