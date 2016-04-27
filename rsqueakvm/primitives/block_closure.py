from rsqueakvm import constants, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.primitives import prim_table, expose_primitive, assert_class
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import jit

# ___________________________________________________________________________
# BlockClosure Primitives

@expose_primitive(CLOSURE_COPY_WITH_COPIED_VALUES,
                  unwrap_spec=[object, int, list])
def func(interp, s_frame, outerContext, numArgs, copiedValues):
    w_context = interp.space.newClosure(outerContext, s_frame.pc(),
                                        numArgs, copiedValues)
    return w_context


def activateClosure(interp, w_block, args_w):
    space = interp.space
    assert_class(interp, w_block, space.w_BlockClosure)
    block = wrapper.BlockClosureWrapper(space, w_block)
    blockNumArgs = jit.promote(block.numArgs())
    if not blockNumArgs == len(args_w):
        raise PrimitiveFailedError()
    outer_ctxt = block.outerContext()
    outer_ctxt_class = jit.promote(outer_ctxt.getclass(space))
    if not (outer_ctxt_class is space.w_MethodContext or
            outer_ctxt_class is space.w_BlockContext):
        raise PrimitiveFailedError()
    assert isinstance(outer_ctxt, W_PointersObject)

    # additionally to the smalltalk implementation, this also pushes
    # args and copiedValues
    s_new_frame = block.create_frame(outer_ctxt, args_w)
    w_closureMethod = s_new_frame.w_method()

    assert isinstance(w_closureMethod, W_CompiledMethod)
    assert w_block is not block.outerContext()

    return s_new_frame


@expose_primitive(CLOSURE_VALUE,
                  unwrap_spec=[object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure):
    return activateClosure(interp, w_block_closure, [])

@expose_primitive(CLOSURE_VALUE_,
                  unwrap_spec=[object, object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0):
    return activateClosure(interp, w_block_closure, [w_a0])

@expose_primitive(CLOSURE_VALUE_VALUE,
                  unwrap_spec=[object, object, object],
                  result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1])

@expose_primitive(CLOSURE_VALUE_VALUE_VALUE,
                  unwrap_spec=[object, object, object, object],
                  result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1, w_a2):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1, w_a2])

@expose_primitive(CLOSURE_VALUE_VALUE_VALUE_VALUE,
                  unwrap_spec=[object, object, object, object, object],
                  result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1, w_a2, w_a3):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1, w_a2, w_a3])

@expose_primitive(CLOSURE_VALUE_WITH_ARGS,
                  unwrap_spec=[object, list], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, args_w):
    return activateClosure(interp, w_block_closure, args_w)

@expose_primitive(CLOSURE_VALUE_NO_CONTEXT_SWITCH,
                  unwrap_spec=[object], result_is_new_frame=True,
                  may_context_switch=False)
def func(interp, s_frame, w_block_closure):
    return activateClosure(interp, w_block_closure, [])

@expose_primitive(CLOSURE_VALUE_NO_CONTEXT_SWITCH_,
                  unwrap_spec=[object, object], result_is_new_frame=True,
                  may_context_switch=False)
def func(interp, s_frame, w_block_closure, w_a0):
    return activateClosure(interp, w_block_closure, [w_a0])

# ___________________________________________________________________________
# Override the default primitive to give latitude to the VM in context management.

@expose_primitive(CTXT_SIZE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_PointersObject):
        if w_rcvr.getclass(interp.space).is_same_object(interp.space.w_MethodContext):
            if w_rcvr.fetch(interp.space, constants.MTHDCTX_METHOD) is interp.space.w_nil:
                # special case: (MethodContext allInstances at: 1) does not have a method. All fields are nil
                return interp.space.wrap_int(0)
            else:
                return interp.space.wrap_int(w_rcvr.as_context_get_shadow(interp.space).stackdepth())
    return interp.space.wrap_int(w_rcvr.varsize())

prim_table[CTXT_AT] = prim_table[AT]
prim_table[CTXT_AT_PUT] = prim_table[AT_PUT]
