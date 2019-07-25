"""
Base functions for ValuePlugin.
"""


from rsqueakvm.model.base import W_Object
from rsqueakvm.error import FatalError, SmalltalkException
from rpython.rlib import rerased, objectmodel, jit, debug


WRITE_OPERATIONS = [
    # W_Object
    'atput0', 'store', 'store_all', 'setword' '_become',
    # 'fillin', 'fillin_weak', 'fillin_finalize', 
    # W_PointersObject
    'pointers_become_one_way',
    # W_BytesObject / W_WordsObject
    'setchar', 'short_atput0', 'setword', 'setwords',
    'convert_to_bytes_layout', 'setbytes', 'mutate'
]


@jit.unroll_safe
def make_sure_all_value(objects_w):
    values_w = [w_object._as_value() for w_object in objects_w]
    debug.make_sure_not_resized(values_w)
    return values_w

# def immutabilize(cls):
#     if hasattr(cls, '_immutable_fields_'):
#         new = [field.translate(None, '?')
#                for field in getattr(cls, '_immutable_fields_')]
#         cls._immutable_fields_ = new

class NoValueError(SmalltalkException):
    def __init__(self):
        SmalltalkException.__init__(self, "This cannot be a value")


class ValueMixin(object):
    def is_value(self):
        return True

    def _as_value(self):
        return self
    def shape(self):
        from .shape import in_storage_shape_instance
        return jit.promote(in_storage_shape_instance())

    def can_become(self, other):
        return False
    def gethash(self):
        return self._valuehash()

    @jit.elidable
    def _valuehash(self):
        return 0 # haha


class W_Value(W_Object):
    _attrs_ = []
    repr_classname = "W_Value"
    objectmodel.import_from_mixin(ValueMixin)

def make_value_class(cls):
    class _Value(cls):
        objectmodel.import_from_mixin(ValueMixin)

    # immutabilize(_Value)
    _Value.__name__ = cls.__name__ + "Value"
    for method_name in WRITE_OPERATIONS:
        if hasattr(_Value, method_name):
            def bail(self, *args):
                raise FatalError
            setattr(_Value, method_name, bail)
    def _as_value(self):
        return _Value(self.value)
    cls._as_value = _as_value
    return _Value
    
def patch():
    patch_w_object()
    patch_primitives()
    
def patch_primitives():
    from rsqueakvm.model.numeric import W_Float, W_SmallInteger
    _Float = make_value_class(W_Float)
    class __extend__(_Float):
        @jit.elidable
        def _valuehash(self):
            from rpython.rlib.rarithmetic import intmask
            return (intmask(objectmodel.compute_hash(self.getvalue())) % 2**22) + 1
    
    _SmallInteger = make_value_class(W_SmallInteger)
    class __extend__(_SmallInteger):
        @jit.elidable
        def _valuehash(self):
            return self.value
    
def patch_w_object():
    """Add `W_Object.is_value` which by default returns `False`."""
    from rsqueakvm.model.base import W_Object

    class __extend__(W_Object):
        def is_value(self):
            return False
        def shape(self):
            # this is not supported
            raise FatalError
        def _as_value(self):
            # this is not supported
            raise NoValueError

def patch_interpreter():
    from rsqueakvm.interpreter import (Interpreter,
                                       jit_driver_name,
                                       get_printable_location,
                                       resume_get_printable_location,
                                       ProcessSwitch, StackOverflow,
                                       LocalReturn, NonLocalReturn,
                                       NonVirtualReturn, FreshReturn)
    from rsqueakvm.storage_contexts import ContextPartShadow

    @jit.unroll_safe
    def _shape_tuple(self):
        from .shape import find_shape_tuple
        from .pointers import W_PointersValue
        count = self.stack_ptr() - 1
        shapes = [None] * count
        for i in range(count):
            w_obj = self.gettemp(i)
            if isinstance(w_obj, W_PointersValue):
                shapes[i] = w_obj._shape
        return find_shape_tuple(shapes)

    ContextPartShadow.shape_tuple = _shape_tuple
    
    def _get_printable_location(pc, self, method, w_class, blockmethod, shapes):
        result = get_printable_location(pc, self, method, w_class, blockmethod)
        result += shapes.merge_point_string()
        return result
    def _resume_get_printable_location(pc, self, method, w_class, shapes):
        result = resume_get_printable_location(pc, self, method, w_class)
        result += shapes.merge_point_string()
        return result

    jit_driver = jit.JitDriver(
        name=jit_driver_name,
        greens=['pc', 'self', 'method', 'w_class', 'blockmethod', 'shapes'],
        reds=['s_context'],
        virtualizables=['s_context'],
        get_printable_location=_get_printable_location,
        is_recursive=True
    )
    Interpreter.jit_driver = jit_driver
    

    resume_driver = jit.JitDriver(
        name=jit_driver_name + "_resume",
        greens=['pc', 'self', 'method', 'w_class', 'shapes'],
        reds=['s_context'],
        # virtualizables=['s_context'],
        get_printable_location=_resume_get_printable_location,
        is_recursive=True
    )

    Interpreter.resume_driver = resume_driver

    def _loop(self, w_active_context):
        # This is the top-level loop and is not invoked recursively.
        s_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            method = s_context.w_method()
            pc = s_context.pc()
            shapes = s_context.shape_tuple()
            self.resume_driver.jit_merge_point(
                pc=pc,
                self=self,
                method=method,
                w_class=self.getreceiverclass(s_context),
                s_context=s_context,
                shapes=shapes)
            s_sender = s_context.s_sender()
            try:
                self.stack_frame(s_context, None, True)
                raise Exception("loop_bytecodes left without raising...")
            except ProcessSwitch, e:
                if self.is_tracing() or self.trace_important:
                    e.print_trace()
                self.process_switch_count += 1
                s_context = e.s_new_context
                if not e.forced:
                    method = s_context.w_method()
                    pc = s_context.pc()
                    shapes = s_context.shape_tuple()
                    self.resume_driver.can_enter_jit(
                        pc=pc,
                        self=self,
                        method=method,
                        w_class=self.getreceiverclass(s_context),
                        s_context=s_context,
                        shapes=shapes)
            except StackOverflow, e:
                if self.is_tracing() or self.trace_important:
                    e.print_trace()
                self.stack_overflow_count += 1
                s_context = e.s_new_context
            except LocalReturn, ret:
                target = s_sender
                s_context = self.unwind_context_chain_local(target, ret.value(self.space), s_context)
                if self.is_tracing() or self.trace_important:
                    print "\n====== Local Return in top-level loop, contexts forced to heap at: %s" % s_context.short_str()
            except NonLocalReturn, ret:
                target = s_sender if ret.arrived_at_target else ret.s_home_context.s_sender() # fine to force here
                s_context = self.unwind_context_chain(s_sender, target, ret.value(self.space), s_context)
                if self.is_tracing() or self.trace_important:
                    print "\n====== Non Local Return in top-level loop, contexts forced to heap at: %s" % s_context.short_str()
            except NonVirtualReturn, ret:
                if self.is_tracing() or self.trace_important:
                    ret.print_trace()
                s_context = self.unwind_context_chain(ret.s_current_context, ret.s_target_context, ret.w_value, s_context)
    Interpreter.loop = _loop

    def _loop_bytecodes(self, s_context, may_context_switch):
        old_pc = 0
        if not jit.we_are_jitted() and may_context_switch:
            self.quick_check_for_interrupt(s_context)
        method = s_context.w_method()
        while True:
            pc = s_context.pc()
            shapes = s_context.shape_tuple()
            if pc < old_pc:
                if jit.we_are_jitted():
                    # Do the interrupt-check at the end of a loop, don't interrupt loops midway.
                    self.jitted_check_for_interrupt(s_context)
                self.jit_driver.can_enter_jit(
                    pc=pc, self=self, method=method,
                    w_class=self.getreceiverclass(s_context),
                    blockmethod=self.getblockmethod(s_context),
                    s_context=s_context,
                    shapes=shapes)
            old_pc = pc
            self.jit_driver.jit_merge_point(
                pc=pc, self=self, method=method,
                w_class=self.getreceiverclass(s_context),
                blockmethod=self.getblockmethod(s_context),
                s_context=s_context,
                shapes=shapes)
            try:
                self.step(s_context)
            except FreshReturn, ret:
                raise ret.exception
            except LocalReturn, ret:
                s_context.push(ret.value(self.space))
            except NonLocalReturn, ret:
                if ret.arrived_at_target:
                    s_context.push(ret.value(self.space))
                else:
                    raise ret

    Interpreter.loop_bytecodes = _loop_bytecodes
