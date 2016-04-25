from rsqueakvm import storage_contexts, constants, wrapper
from rsqueakvm.error import PrimitiveFailedError, PrimitiveNotYetWrittenError
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.numeric import W_Float, W_LargePositiveInteger1Word
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.primitives import expose_primitive, assert_pointers, assert_class
from rsqueakvm.primitives.bytecodes import *
from rsqueakvm.primitives.misc import fake_bytes_left

from rpython.rlib import jit, objectmodel
from rpython.rtyper.extregistry import ExtRegistryEntry


# ___________________________________________________________________________
# Failure

@expose_primitive(FAIL, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        w_message = None
        if s_frame.w_method().lookup_selector == 'doesNotUnderstand:':
            w_arguments = s_frame.w_arguments()
            if len(w_arguments) >= 1:
                w_message = w_arguments[0]
        s_frame.exitFromHeadlessExecution(w_message=w_message)
    raise PrimitiveFailedError()

# ___________________________________________________________________________
# Control Primitives

@expose_primitive(EQUIVALENT, unwrap_spec=[object, object])
def func(interp, s_frame, w_arg, w_rcvr):
    return interp.space.wrap_bool(w_arg.is_same_object(w_rcvr))

@expose_primitive(CLASS, unwrap_spec=None)
def func(interp, s_frame, argcount):
    w_obj = s_frame.pop()
    if argcount == 1:
        s_frame.pop()  # receiver, e.g. ContextPart>>objectClass:
    return w_obj.getclass(interp.space)

@expose_primitive(BYTES_LEFT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return fake_bytes_left(interp)

@expose_primitive(QUIT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    from rsqueakvm.error import Exit
    raise Exit('Quit-Primitive called')

@expose_primitive(EXIT_TO_DEBUGGER, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        s_frame.exitFromHeadlessExecution("EXIT_TO_DEBUGGER")
    raise PrimitiveNotYetWrittenError()

@expose_primitive(CHANGE_CLASS, unwrap_spec=[object, object], no_result=True)
def func(interp, s_frame, w_arg, w_rcvr):
    w_arg_class = w_arg.getclass(interp.space)
    w_rcvr_class = w_rcvr.getclass(interp.space)

    # We should fail if:

    # 1. Rcvr or arg are SmallIntegers
    if (w_arg_class.is_same_object(interp.space.w_SmallInteger) or
        w_rcvr_class.is_same_object(interp.space.w_SmallInteger)):
        raise PrimitiveFailedError()

    # 2. Rcvr is an instance of a compact class and argument isn't
    # or vice versa XXX we don't have to fail here, but for squeak it's a problem

    # 3. Format of rcvr is different from format of argument
    if ((isinstance(w_arg, W_PointersObject) and
         isinstance(w_rcvr, W_PointersObject)) or
        (isinstance(w_arg, W_BytesObject) and
         isinstance(w_rcvr, W_BytesObject)) or
        (isinstance(w_arg, W_WordsObject) and
         isinstance(w_rcvr, W_WordsObject))):
        w_rcvr.change_class(interp.space, w_arg_class)
        return w_rcvr
    else:
        # TODO: this should also work to change bytes to words and such
        raise PrimitiveNotYetWrittenError

@expose_primitive(EXTERNAL_CALL, clean_stack=False, no_result=True,
                  compiled_method=True)
def func(interp, s_frame, argcount, w_method):
    space = interp.space
    w_description = w_method.literalat0(space, 1)
    if (not isinstance(w_description, W_PointersObject) or
            w_description.size() < 2):
        raise PrimitiveFailedError
    w_modulename = jit.promote(w_description.at0(space, 0))
    w_functionname = jit.promote(w_description.at0(space, 1))
    if w_modulename is space.w_nil:
        """
        CompiledMethod allInstances select: [:cm | cm primitive = 117 and: [cm literals first first isNil]].
        There are no interesting named module-less primitives among those 28
        found in Squeak 5. They either have proper fallback or just don't work on
        Cog either.
        """
        raise PrimitiveFailedError

    if not (isinstance(w_modulename, W_BytesObject) and
            isinstance(w_functionname, W_BytesObject)):
        raise PrimitiveFailedError
    signature = (space.unwrap_string(w_modulename), space.unwrap_string(w_functionname))

    if (not constants.IS_64BIT) and interp.space.use_plugins.is_set():
        from rsqueakvm.plugins.squeak_plugin_proxy import IProxy, MissingPlugin
        try:
            return IProxy.call(signature, interp, s_frame, argcount, w_method)
        except MissingPlugin:
            pass

    if False: pass  # just elifs
    elif signature[0] == 'LargeIntegers':
        from rsqueakvm.plugins.large_integer import LargeIntegerPlugin
        return LargeIntegerPlugin.call(signature[1], interp, s_frame,
                                       argcount, w_method)
    elif signature[0] == 'MiscPrimitivePlugin':
        from rsqueakvm.plugins.misc import MiscPrimitivePlugin
        return MiscPrimitivePlugin.call(signature[1], interp, s_frame,
                                        argcount, w_method)
    elif signature[0] == "SocketPlugin":
        from rsqueakvm.plugins.socket import SocketPlugin
        return SocketPlugin.call(signature[1], interp, s_frame, argcount,
                                 w_method)
    elif signature[0] == "FilePlugin":
        from rsqueakvm.plugins.fileplugin import FilePlugin
        return FilePlugin.call(signature[1], interp, s_frame, argcount,
                               w_method)
    elif signature[0] == "VMDebugging":
        from rsqueakvm.plugins.vmdebugging import DebuggingPlugin
        return DebuggingPlugin.call(signature[1], interp, s_frame, argcount,
                                    w_method)
    else:
        from rsqueakvm.plugins.simulation import SimulationPlugin
        return SimulationPlugin.simulate(w_functionname, signature, interp,
                                         s_frame, argcount, w_method)

@expose_primitive(COMPILED_METHOD_FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    w_class = w_rcvr.compiled_in()
    if w_class:
        w_class = assert_pointers(w_class)
        w_class.as_class_get_shadow(interp.space).flush_method_caches()
    return w_rcvr

@objectmodel.specialize.arg(0)
def walk_gc_references(func, gcrefs):
    from rpython.rlib import rgc
    for gcref in gcrefs:
        if gcref and not rgc.get_gcflag_extra(gcref):
            try:
                rgc.toggle_gcflag_extra(gcref)
                func(gcref)
                walk_gc_references(func, rgc.get_rpy_referents(gcref))
            finally:
                rgc.toggle_gcflag_extra(gcref)

@objectmodel.specialize.arg(0)
def walk_gc_objects(func):
    from rpython.rlib import rgc
    walk_gc_references(func, rgc.get_rpy_roots())

@objectmodel.specialize.arg(0, 1)
def walk_gc_objects_of_type(type, func):
    from rpython.rlib import rgc
    def check_type(gcref):
        w_obj = rgc.try_cast_gcref_to_instance(type, gcref)
        if w_obj:
            func(w_obj)
    walk_gc_objects(check_type)

@expose_primitive(SYMBOL_FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    # No need to do this, method dictionaries invalidate their traces as needed
    return w_rcvr

@expose_primitive(BLOCK_COPY, unwrap_spec=[object, int])
def func(interp, s_frame, w_context, argcnt):
    # From B.B.: If receiver is a MethodContext, then it becomes
    # the new BlockContext's home context.  Otherwise, the home
    # context of the receiver is used for the new BlockContext.
    # Note that in our impl, MethodContext.w_home == self
    w_context = assert_pointers(w_context)
    s_method_context = w_context.as_context_get_shadow(interp.space).s_home()

    # The block bytecodes are stored inline: so we skip past the
    # bytecodes to invoke this primitive to get to them.
    initialip = s_frame.pc() + 2
    s_new_context = storage_contexts.ContextPartShadow.build_block_context(
            interp.space, s_method_context, argcnt, initialip)
    return s_new_context.w_self()

@expose_primitive(VALUE, result_is_new_frame=True)
def func(interp, s_frame, argument_count):
    # argument_count does NOT include the receiver.
    # This means that for argument_count == 3 the stack looks like:
    #  3      2       1      Top
    #  Rcvr | Arg 0 | Arg1 | Arg 2
    #
    # Validate that we have a block on the stack and that it received
    # the proper number of arguments:
    w_block_ctx = s_frame.peek(argument_count)

    # XXX need to check this since VALUE is called on all sorts of objects.
    if not w_block_ctx.getclass(interp.space).is_same_object(
            interp.space.w_BlockContext):
        raise PrimitiveFailedError()

    w_block_ctx = assert_pointers(w_block_ctx)
    s_block_ctx = w_block_ctx.as_context_get_shadow(interp.space)

    exp_arg_cnt = s_block_ctx.expected_argument_count()
    if argument_count != exp_arg_cnt:  # exp_arg_cnt doesn't count self
        raise PrimitiveFailedError()

    # Initialize the block stack with the arguments that were
    # pushed.  Also pop the receiver.
    block_args = s_frame.pop_and_return_n(exp_arg_cnt)

    # Reset stack of blockcontext to []
    s_block_ctx.reset_stack()
    s_block_ctx.push_all(block_args)

    s_frame.pop()
    s_block_ctx.reset_pc()
    return s_block_ctx

@expose_primitive(VALUE_WITH_ARGS, unwrap_spec=[object, list],
                  result_is_new_frame=True)
def func(interp, s_frame, w_block_ctx, args_w):

    w_block_ctx = assert_pointers(w_block_ctx)
    s_block_ctx = w_block_ctx.as_context_get_shadow(interp.space)
    exp_arg_cnt = s_block_ctx.expected_argument_count()

    if len(args_w) != exp_arg_cnt:
        raise PrimitiveFailedError()

    # Push all the items from the array
    for i in range(exp_arg_cnt):
        s_block_ctx.push(args_w[i])

    # XXX Check original logic. Image does not test this anyway
    # because falls back to value + internal implementation
    s_block_ctx.reset_pc()
    return s_block_ctx

@expose_primitive(PERFORM)
def func(interp, s_frame, argcount):
    raise PrimitiveFailedError()

@expose_primitive(PERFORM_WITH_ARGS,
                  unwrap_spec=[object, object, list],
                  no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr, w_selector, w_arguments):
    s_frame.pop_n(2)  # removing our arguments
    return s_frame._sendSelector(w_selector, len(w_arguments), interp, w_rcvr,
                                 w_rcvr.class_shadow(interp.space),
                                 w_arguments=w_arguments)

@expose_primitive(WITH_ARGS_EXECUTE_METHOD,
                  result_is_new_frame=True, unwrap_spec=[object, list, object])
def func(interp, s_frame, w_rcvr, args_w, w_cm):
    if not isinstance(w_cm, W_CompiledMethod):
        raise PrimitiveFailedError()
    code = w_cm.primitive()
    if code:
        raise PrimitiveFailedError("withArgs:executeMethod: not support with primitive method")
    return w_cm.create_frame(interp.space, w_rcvr, args_w)


# XXX we might want to disable the assert_class checks in the 4 primitives below

@expose_primitive(SIGNAL,
                  unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Semaphore)
    wrapper.SemaphoreWrapper(interp.space, w_rcvr).signal(s_frame)

@expose_primitive(WAIT,
                  unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Semaphore)
    wrapper.SemaphoreWrapper(interp.space, w_rcvr).wait(s_frame)

@expose_primitive(RESUME,
                  unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Process)
    wrapper.ProcessWrapper(interp.space, w_rcvr).resume(s_frame)

@expose_primitive(SUSPEND,
                  unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Process)
    proc = wrapper.ProcessWrapper(interp.space, w_rcvr)
    s_frame.pop()  # remove receiver
    s_frame.push(proc.my_list())  # leave my_list on stack as return value
    proc.suspend(s_frame)

@expose_primitive(YIELD,
                  unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    # we leave the rcvr on the stack, so it is there even if we resume another
    # process when yielding
    w_process = wrapper.SchedulerWrapper(interp.space, w_rcvr).active_process()
    wrapper.ProcessWrapper(interp.space, w_process).yield_(s_frame)

@expose_primitive(EXIT_CRITICAL_SECTION,
                  unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    # we leave the rcvr on the stack, so it is there even if we resume another
    # process immediately when exiting the critical section
    wrapper.CriticalSectionWrapper(interp.space, w_rcvr).exit(s_frame)

@expose_primitive(ENTER_CRITICAL_SECTION,
                  unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert s_frame.pop() is w_rcvr
    # we take pop the receiver and push the return value of this primitive
    # inside the wrapper code, so it is there regardless of if we have to wait
    # or not
    return wrapper.CriticalSectionWrapper(interp.space, w_rcvr).enter(s_frame)

@expose_primitive(TEST_AND_SET_OWNERSHIP_OF_CRITICAL_SECTION,
                  unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return wrapper.CriticalSectionWrapper(
        interp.space, w_rcvr).test_and_set_owner(s_frame)

@expose_primitive(FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    w_rcvr = assert_pointers(w_rcvr)
    s_class = w_rcvr.as_class_get_shadow(interp.space)
    s_class.flush_method_caches()
    return w_rcvr

def model_sizeof(model):
    return 4  # constant for interpretation

class Entry(ExtRegistryEntry):
    _about_ = model_sizeof

    def compute_result_annotation(self, s_model):
        from rpython.annotator.model import SomeInteger
        return SomeInteger(nonneg=True, knowntype=int)

    def specialize_call(self, hop):
        from rpython.rtyper.lltypesystem import lltype
        from rpython.memory.lltypelayout import sizeof
        modelrepr = hop.rtyper.getrepr(hop.args_s[0])
        hop.exception_cannot_occur()
        sz = sizeof(modelrepr.lowleveltype.TO, 1)
        # Also add the instance-specific helper structs that all instances have
        if modelrepr.rclass.classdef.classdesc.pyobj is W_PointersObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst__storage"].TO, 0)
            # not adding shadows, because these are often shared
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_WordsObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_words"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_words"].TO, 0)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_BytesObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_version"].TO)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_CompiledMethod:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_version"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_literals"].TO, 0)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_version"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_lookup_selector"].TO, 0)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_Float:
            pass
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_LargePositiveInteger1Word:
            pass
        return hop.inputconst(lltype.Signed, sz)

@expose_primitive(BYTE_SIZE_OF_INSTANCE)
def func(interp, s_frame, argcount):
    from rpython.memory.lltypelayout import sizeof
    from rsqueakvm.storage_classes import POINTERS,\
        WEAK_POINTERS, WORDS, BYTES, COMPILED_METHOD,\
        FLOAT, LARGE_POSITIVE_INTEGER
    # This does not count shadows and the memory required for storage or any of
    # that "meta-info", but only the size of the types struct and the requested
    # fields.
    if argcount == 1:
        size = interp.space.unwrap_int(s_frame.pop())
    else:
        size = 0
    w_rcvr = s_frame.pop()

    if argcount > 1:
        raise PrimitiveFailedError

    s_class = w_rcvr.as_class_get_shadow(interp.space)
    instance_kind = s_class.get_instance_kind()
    if instance_kind in [POINTERS, WEAK_POINTERS]:
        r = model_sizeof(objectmodel.instantiate(W_PointersObject)) + (s_class.instsize() + size) * constants.BYTES_PER_WORD
    elif instance_kind == WORDS:
        r = model_sizeof(objectmodel.instantiate(W_WordsObject)) + size * constants.BYTES_PER_WORD
    elif instance_kind == BYTES:
        r = model_sizeof(objectmodel.instantiate(W_BytesObject)) + size
    elif instance_kind == COMPILED_METHOD:
        r = model_sizeof(objectmodel.instantiate(W_CompiledMethod))
    elif instance_kind == FLOAT:
        r = model_sizeof(objectmodel.instantiate(W_Float))
    elif instance_kind == LARGE_POSITIVE_INTEGER:
        if size <= 4:
            r = model_sizeof(objectmodel.instantiate(W_LargePositiveInteger1Word))
        else:
            r = model_sizeof(objectmodel.instantiate(W_BytesObject)) + size
    else:
        raise PrimitiveFailedError
    return interp.space.wrap_int(r)


# ___________________________________________________________________________
# Quick Push Const Primitives

@expose_primitive(PUSH_SELF, unwrap_spec=[object])
def func(interp, s_frame, w_self):
    # no-op really
    return w_self

def make_push_const_func(code, name):
    @expose_primitive(code, unwrap_spec=[object])
    def func(interp, s_frame, w_ignored):
        return getattr(interp.space, name)
    return func

for (code, name) in [
    (PUSH_TRUE, "w_true"),
    (PUSH_FALSE, "w_false"),
    (PUSH_NIL, "w_nil"),
    (PUSH_MINUS_ONE, "w_minus_one"),
    (PUSH_ZERO, "w_zero"),
    (PUSH_ONE, "w_one"),
    (PUSH_TWO, "w_two"),
        ]:
    make_push_const_func(code, name)
