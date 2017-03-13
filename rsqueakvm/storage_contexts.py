from rsqueakvm import constants, error, wrapper
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.block_closure import W_BlockClosure
from rsqueakvm.storage import AbstractStrategy, ShadowMixin, StrategyMetaclass

from rpython.rlib import jit, objectmodel, unroll
from rpython.rlib.objectmodel import import_from_mixin, always_inline
from rpython.rlib.rarithmetic import r_uint, intmask
from rpython.rlib.rstrategies import rstrategies as rstrat
from rpython.tool.pairtype import extendabletype


@objectmodel.specialize.call_location()
def fresh_virtualizable(x):
    return jit.hint(x, access_directly=True, fresh_virtualizable=True)

class ContextState(object):
    _attrs_ = ["name", "i"]
    _immutable_fields_ = ["name", "i"]
    states = []
    def __init__(self, name):
        self.name = name
        self.i = len(self.states)
        self.states.append(self)
    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name
    def num(self):
        return self.i
InactiveContext = ContextState("InactiveContext")
ActiveContext = ContextState("ActiveContext")
DirtyContext = ContextState("DirtyContext")

class ExtendableStrategyMetaclass(extendabletype, StrategyMetaclass):
    pass


class ExtraContextAttributes(object):
    _attrs_ = [
        # Cache for allInstances
        'instances_w',
        # Fallback for failed primitives
        '_s_fallback',
        # From block-context
        '_w_home', '_initialip', '_eargc']

    def __init__(self):
        self.instances_w = None
        self._s_fallback = None
        self._w_home = None
        self._initialip = 0
        self._eargc = 0


state_mask     = r_uint(0b00111111111111111111111111111111)
stackptr_mask  = r_uint(0b11000000001111111111111111111111)
pc_mask        = r_uint(0b11111111110000000000000000000000)
returned_pc    = 0b00000000001111111111111111111111
state_shift    = 30
stackptr_shift = 22
pc_shift       = 0


class ContextPartShadow(AbstractStrategy):
    """
    This Shadow handles the entire object storage on its own, ignoring the _storage
    field in W_PointersObject. The w_self parameter in fetch/store/size etc. is ignored,
    and the own_fetch/own_store/own_size methods from ShadowMixin should be used instead.
    This shadow can exist without a W_PointersObject.
    In order to integrate well with the RPython toolchain (virtualizables and jit), this
    class actually represents one of two classes, determined by the is_block_context switch.
    """
    repr_classname = "ContextPartShadow"

    __metaclass__ = ExtendableStrategyMetaclass
    import_from_mixin(ShadowMixin)

    _attrs_ = ['_w_self', '_w_self_size',
               '_state_stackptr_pc',
               # Core context data
               '_s_sender', '_temps_and_stack',
               # MethodContext data
               'blockmethod',
               'closure', '_w_receiver', '_w_method',
               # Extra data
               'extra_data'
               ]

    _virtualizable_ = [
        "_w_self", "_w_self_size",
        '_state_stackptr_pc',
        '_s_sender', "_temps_and_stack[*]",
        'blockmethod',
        'closure', '_w_receiver', '_w_method',
        'extra_data'
    ]

    # ______________________________________________________________________
    # Initialization

    @jit.unroll_safe
    def __init__(self, space, w_self, size, w_class):
        self = fresh_virtualizable(self)
        AbstractStrategy.__init__(self, space, w_self, size, w_class)

        self._state_stackptr_pc = r_uint(0)
        self._s_sender = jit.vref_None
        if w_self is not None:
            self._w_self_size = w_self.size()
        else:
            self._w_self_size = size
        self._w_self = w_self
        self.set_state(InactiveContext)
        self.store_pc(0)

        # From MethodContext
        self.blockmethod = None
        self.closure = None
        self._w_method = None
        self._w_receiver = None
        # Extra data
        self.extra_data = None

    def _initialize_storage(self, w_self, initial_size):
        # The context object holds all of its storage itself.
        self.set_storage(w_self, None)

    @jit.unroll_safe
    def _convert_storage_from(self, w_self, previous_strategy):
        size = previous_strategy.size(w_self)
        # Some fields have to be initialized before the rest,
        # to ensure correct initialization.
        if self.pure_is_block_context():
            assert len(self.privileged_block_fields) == 1
            self.store(w_self, self.privileged_block_fields[0],
                       previous_strategy.fetch(w_self, self.privileged_block_fields[0]))
        else:
            assert len(self.privileged_method_fields) == 2
            self.store(w_self, self.privileged_method_fields[0],
                       previous_strategy.fetch(w_self, self.privileged_method_fields[0]))
            self.store(w_self, self.privileged_method_fields[1],
                       previous_strategy.fetch(w_self, self.privileged_method_fields[1]))

        # Now the temp size will be known.
        self.init_temps_and_stack()

        # After this, convert the rest of the fields.
        for n0 in range(size):
            if not self.is_privileged_index(n0):
                self.store(w_self, n0, previous_strategy.fetch(w_self, n0))

        # Finally, we can remove the storage that was there previously
        self._initialize_storage(w_self, size)

    privileged_block_fields = (constants.BLKCTX_HOME_INDEX,)
    privileged_method_fields = (constants.MTHDCTX_METHOD, constants.MTHDCTX_CLOSURE_OR_NIL)

    def is_privileged_index(self, n0):
        if self.pure_is_block_context():
            assert len(self.privileged_block_fields) == 1
            return n0 == self.privileged_block_fields[0]
        else:
            assert len(self.privileged_method_fields) == 2
            # we use | instead of || to generate only one guard
            return (n0 == self.privileged_method_fields[0]) | (n0 == self.privileged_method_fields[1])

    def get_extra_data(self):
        if self.extra_data is None:
            self.extra_data = ExtraContextAttributes()
        return self.extra_data

    def get_fallback(self):
        if self.extra_data is None:
            return None
        else:
            return self.extra_data._s_fallback

    @always_inline
    def _get_state_stackptr_pc(self):
        return jit.promote(self._state_stackptr_pc)

    @always_inline
    def get_state(self):
        return ContextState.states[intmask((self._get_state_stackptr_pc() & ~state_mask) >> state_shift)]

    @always_inline
    def set_state(self, t):
        self._state_stackptr_pc = (self._get_state_stackptr_pc() & state_mask) | (r_uint(t.num()) << state_shift)
        assert self.get_state() == t

    # ______________________________________________________________________
    # Accessing object fields

    def pure_is_block_context(self):
        if self.space.uses_block_contexts.is_set():
            return self.space.w_BlockContext.is_same_object(self.w_class)
        else:
            return False

    def size(self, ignored_w_self):
        return self._w_self_size

    def fetch(self, ignored_w_self, n0):
        if self.pure_is_block_context():
            return self.fetch_block_context(n0)
        else:
            return self.fetch_method_context(n0)

    def fetch_context_part(self, n0):
        if n0 == constants.CTXPART_SENDER_INDEX:
            return self.w_sender()
        if n0 == constants.CTXPART_PC_INDEX:
            return self.wrap_pc()
        if n0 == constants.CTXPART_STACKP_INDEX:
            return self.wrap_stackpointer()
        if self.stackstart() <= n0 < self.external_stackpointer():
            temp_i = self.stackdepth() - (n0-self.stackstart()) - 1
            assert temp_i >= 0, "trying to access temp out of range"
            return self.peek(temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return self.space.w_nil
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")

    def store(self, ignored_w_self, n0, w_value):
        if self.pure_is_block_context():
            return self.store_block_context(n0, w_value)
        else:
            return self.store_method_context(n0, w_value)

    def store_context_part(self, n0, w_value):
        if n0 == constants.CTXPART_SENDER_INDEX:
            assert isinstance(w_value, W_PointersObject), "trying to store non-pointer sender"
            if w_value.is_nil(self.space):
                self.remove_s_sender()
                if self.get_state() is ActiveContext:
                    self.set_state(DirtyContext)
            else:
                self.store_s_sender(w_value.as_context_get_shadow(self.space))
            return
        if n0 == constants.CTXPART_PC_INDEX:
            return self.store_unwrap_pc(w_value)
        if n0 == constants.CTXPART_STACKP_INDEX:
            return self.unwrap_store_stackpointer(w_value)
        if self.stackstart() <= n0 < self.external_stackpointer():  # XXX can be simplified?
            temp_i = self.stackdepth() - (n0-self.stackstart()) - 1
            assert temp_i >= 0, "trying to store temp out of range"
            return self.set_top(w_value, temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")

    # === Sender ===

    def remove_s_sender(self):
        self._s_sender = jit.vref_None

    def has_s_sender(self):
        return self._s_sender is not jit.vref_None

    def store_s_sender(self, s_sender):
        assert s_sender is not None, "trying to store None s_sender"
        self._s_sender = jit.non_virtual_ref(s_sender)
        if self.get_state() is ActiveContext:
            self.set_state(DirtyContext)

    def enter_virtual_frame(self, s_sender):
        self.set_state(ActiveContext)
        if self.has_s_sender() or s_sender is None:
            return jit.vref_None
        else:
            self._s_sender = jit.virtual_ref(s_sender)
            return self._s_sender

    def leave_virtual_frame(self, vref, ref):
        self.set_state(InactiveContext)
        if vref is not jit.vref_None:
            jit.virtual_ref_finish(vref, ref)
            if vref is self._s_sender:
                # If this frame was not manipulated, _s_sender is still our
                # initial vref. If the frame was manipulated (or has no sender)
                # _s_sender is already a non_virtual_ref.
                self._s_sender = jit.non_virtual_ref(ref)

    def w_sender(self):
        sender = self.s_sender()
        if sender is None:
            return self.space.w_nil
        return sender.w_self()

    def s_sender(self):
        return self._s_sender()

    # === Stack Pointer ===

    def unwrap_store_stackpointer(self, w_sp1):
        # the stackpointer in the W_PointersObject starts counting at the
        # tempframe start
        # Stackpointer from smalltalk world == stacksize in python world
        self.store_stackpointer(self.space.unwrap_int(w_sp1))

    @jit.unroll_safe
    def store_stackpointer(self, size):
        depth = self.stackdepth()
        if size < depth:
            assert size >= 0, "trying to store negative stackpointer"
            self.pop_n(depth - size)
        else:
            self.store_stack_ptr(size)

    def stackdepth(self):
        return self.stack_ptr()

    @always_inline
    def stack_ptr(self):
        return intmask((self._get_state_stackptr_pc() & ~stackptr_mask) >> stackptr_shift)

    @always_inline
    def store_stack_ptr(self, ptr):
        assert ptr >= 0
        assert ptr < 256
        self._state_stackptr_pc = (self._get_state_stackptr_pc() & stackptr_mask) | (r_uint(ptr) << stackptr_shift)
        assert self.stack_ptr() == ptr

    def wrap_stackpointer(self):
        return self.space.wrap_smallint_unsafe(self.stackdepth())

    # === Program Counter ===

    def store_unwrap_pc(self, w_pc):
        if w_pc.is_nil(self.space):
            self.store_pc(returned_pc)
        else:
            pc = self.space.unwrap_int(w_pc)
            pc -= self.w_method().bytecodeoffset()
            pc -= 1
            self.store_pc(pc)

    def wrap_pc(self):
        pc = self.pc()
        if pc == returned_pc:
            return self.space.w_nil
        else:
            pc += 1
            pc += self.w_method().bytecodeoffset()
            return self.space.wrap_smallint_unsafe(pc)

    @always_inline
    def pc(self):
        return intmask((self._get_state_stackptr_pc() & ~pc_mask) >> pc_shift)

    @always_inline
    def store_pc(self, newpc):
        assert newpc >= 0, "trying to store pc < 0"
        assert newpc <= returned_pc, "trying to store pc > returned_pc"
        self._state_stackptr_pc = (self._get_state_stackptr_pc() & pc_mask) | (r_uint(newpc) << pc_shift)
        assert self.pc() == newpc

    # ______________________________________________________________________
    # Specialized accessors
    #
    # These methods have different versions depending on whether the receiver
    # is a BlockContext or MethodContext. The call is forwarded to a specialized
    # version, like a manual kind of inheritance.

    def s_home(self):
        if self.pure_is_block_context():
            return self.s_home_block_context()
        else:
            return self.s_home_method_context()

    def stackstart(self):
        if self.pure_is_block_context():
            return self.stackstart_block_context()
        else:
            return self.stackstart_method_context()

    def w_receiver(self):
        if self.pure_is_block_context():
            return self.w_receiver_block_context()
        else:
            return self.w_receiver_method_context()

    def w_method(self):
        if self.pure_is_block_context():
            return self.w_method_block_context()
        else:
            return self.w_method_method_context()

    def tempsize(self):
        if self.pure_is_block_context():
            return self.tempsize_block_context()
        else:
            return self.tempsize_method_context()

    def is_closure_context(self):
        if self.pure_is_block_context():
            return self.is_closure_context_block_context()
        else:
            return self.is_closure_context_method_context()

    def is_BlockClosure_ensure(self):
        if self.pure_is_block_context():
            return False
        else:
            # Primitive 198 is a marker used in BlockClosure >> ensure:
            return self.w_method().primitive() == 198

    def home_is_self(self):
        if self.pure_is_block_context():
            return self.home_is_self_block_context()
        else:
            return self.home_is_self_method_context()

    # ______________________________________________________________________
    # Temporary Variables
    #
    # Every context has it's own stack. BlockContexts share their temps with
    # their home contexts. MethodContexts created from a BlockClosure get their
    # temps copied from the closure upon activation. Changes are not propagated back;
    # this is handled by the compiler by allocating an extra Array for temps.

    def gettemp(self, index):
        if self.pure_is_block_context():
            return self.gettemp_block_context(index)
        else:
            return self.gettemp_method_context(index)

    def settemp(self, index, w_value):
        if self.pure_is_block_context():
            return self.settemp_block_context(index, w_value)
        else:
            return self.settemp_method_context(index, w_value)

    # === Other properties of Contexts ===

    def mark_returned(self):
        self.store_pc(returned_pc)
        self.remove_s_sender()

    def is_returned(self):
        return self.pc() == returned_pc and (not self.has_s_sender())

    def external_stackpointer(self):
        return self.stackdepth() + self.stackstart()

    def stackend(self):
        # XXX this is incorrect when there is subclassing
        return self._w_self_size

    def fetch_next_bytecode(self):
        pc = jit.promote(self.pc())
        assert pc < returned_pc, "fetching bytecode on returned method"
        self.store_pc(pc + 1)
        return self.fetch_bytecode(pc)

    def fetch_bytecode(self, pc):
        bytecode = self.w_method().fetch_bytecode(pc)
        return ord(bytecode)

    # ______________________________________________________________________
    # Stack Manipulation

    def stacksize(self):
        return self.stackend() - self.stackstart()

    def full_stacksize(self):
        if not self.pure_is_block_context():
            method = self.w_method()
            assert isinstance(method, W_CompiledMethod), "context method is not a W_CompiledMethod"
            stacksize = method.frame_size()
        else:
            stacksize = self.stacksize()  # no temps
        return stacksize

    def init_temps_and_stack(self):
        self = fresh_virtualizable(self)
        stacksize = self.full_stacksize()
        self._temps_and_stack = [None] * stacksize
        tempsize = self.tempsize()
        self.store_stack_ptr(tempsize)  # we point after the last element

    def stack_get(self, index0):
        assert index0 >= 0, "trying to stack_get negative index"
        return self._temps_and_stack[index0] or self.space.w_nil

    def stack_put(self, index0, w_val):
        assert w_val is not None, "trying to put None on the stack"
        assert index0 >= 0, "trying to stack_put at negative index"
        self._temps_and_stack[index0] = w_val

    @objectmodel.not_rpython # this is only for testing.
    def stack(self):
        return self._temps_and_stack[self.tempsize():self.stack_ptr()]

    def pop(self):
        # HACK HACK HACK (3 times)
        # Popping the empty stack is actually an error, but this sometimes
        # happens in strange code paths. So, there's typically only the receiver
        # at bottom, under the stack, or among the temps that could be around there,
        # so just hard return the receiver.
        #assert self.stack_ptr() > self.tempsize()
        ptr = jit.promote(self.stack_ptr()) - 1
        if ptr < 0:
            ret = self.w_receiver()
            ptr = 0
        else:
            ret = self.stack_get(ptr)
        #
        # HACK HACK HACK HACK HACK (5 times)
        # We really would like to nil out the popp'ed value, but there are
        # methods (yes, ContextPart>>contextOn:do:, I am looking at you!) that
        # do a pop and access the stack afterwards. Or better said, do a pop on
        # the empty stack and would hence nil out a temp. We cannot let this
        # happen.
        # Problem: how do we tell the GC to collect outside self.stack_ptr()?
        #  self.stack_put(ptr, self.space.w_nil)
        assert ptr >= 0, "stack pointer got negative in pop"
        self.store_stack_ptr(ptr)
        return ret

    def push(self, w_v):
        ptr = jit.promote(self.stack_ptr())
        self.stack_put(ptr, w_v)
        self.store_stack_ptr(ptr + 1)
        if not jit.we_are_jitted():
            self.w_method().update_frame_size(ptr + 1)

    @jit.unroll_safe
    def push_all(self, lst):
        for elt in lst:
            self.push(elt)

    def top(self):
        return self.peek(0)

    def set_top(self, value, position=0):
        ptr = self.stack_ptr() - position - 1
        self.stack_put(ptr, value)

    def peek(self, idx):
        ptr = jit.promote(self.stack_ptr()) - idx - 1
        return self.stack_get(ptr)

    @jit.unroll_safe
    def peek_n(self, n):
        result = [self.peek(i) for i in range(n - 1, -1, -1)]
        return result

    @jit.unroll_safe
    def pop_n(self, n):
        jit.promote(self.stack_ptr())
        while n > 0:
            n -= 1
            assert self.stack_ptr() >= 1, "stack pointer reduced to < 1 in pop_n"
            self.store_stack_ptr(self.stack_ptr() - 1)
            self.stack_put(self.stack_ptr(), self.space.w_nil)

    @jit.unroll_safe
    def pop_and_return_n(self, n):
        result = [self.peek(i) for i in range(n - 1, -1, -1)]
        self.pop_n(n)
        return result

    # ______________________________________________________________________
    # Primitive support

    def store_instances_array(self, w_class, match_w):
        # used for primitives 77 & 78
        if self.get_extra_data().instances_w is None:
            self.get_extra_data().instances_w = {}
        self.get_extra_data().instances_w[w_class] = match_w

    def instances_array(self, w_class):
        if self.get_extra_data().instances_w is None:
            return None
        else:
            return self.get_extra_data().instances_w.get(w_class, None)

    # ______________________________________________________________________
    # Printing

    def w_arguments(self):
        if self.pure_is_block_context():
            return self.w_arguments_block_context()
        else:
            return self.w_arguments_method_context()

    def method_str(self):
        if self.pure_is_block_context():
            return self.method_str_block_context()
        else:
            return self.method_str_method_context()

    def argument_strings(self):
        return [w_arg.as_repr_string() for w_arg in self.w_arguments()]

    def __str__(self):
        retval = self.short_str()
        retval += "\n%s" % self.w_method().bytecode_string(markBytecode=self.pc() + 1)
        retval += "\nArgs:----------------"
        argcount = self.w_method().argsize
        j = 0
        for w_obj in self._temps_and_stack[:self.stack_ptr()]:
            w_obj = self.space.w_nil if w_obj is None else w_obj
            if j == argcount:
                retval += "\nTemps:---------------"
            if j == self.tempsize():
                retval += "\nStack:---------------"
            retval += "\n  %0.2i: %s" % (j, w_obj.as_repr_string())
            j += 1
        retval += "\n---------------------"
        return retval

    def short_str(self):
        arg_strings = self.argument_strings()
        if len(arg_strings) > 0:
            args = " , ".join(arg_strings)
            args = " (%d arg(s): %s)" % (len(arg_strings), args)
        else:
            args = ""
        return '%s [pc: %d] (rcvr: %s)%s' % (
            self.method_str(),
            self.pc() + 1,
            self.w_receiver().as_repr_string(),
            args
        )

    def print_stack(self, method=True):
        return self.print_padded_stack(method)[1]

    def print_padded_stack(self, method):
        padding = ret_str = ''
        if self.has_s_sender():
            padding, ret_str = self.s_sender().print_padded_stack(method)
        if method:
            desc = self.method_str()
        else:
            desc = self.short_str()
        return padding + ' ', '%s\n%s%s' % (ret_str, padding, desc)

    def exitFromHeadlessExecution(self, selector="", w_message=None):
        if not objectmodel.we_are_translated():
            if getattr(self.space, "testing", False):
                return  # During Testing
            do_exit = True
            import pdb; pdb.set_trace()
            if not do_exit: # during debugging, we can change do_exit and go on
                return
        print "== Receiver: %s" % self.w_receiver().as_repr_string()
        if isinstance(w_message, W_PointersObject):
            fields = w_message.fetch_all(self.space)
            if len(fields) >= 1:
                print "== Selector: %s" % fields[0].selector_string()
            if len(fields) >= 2:
                w_args = fields[1]
                if isinstance(w_args, W_PointersObject):
                    arg_strings = [w_arg.selector_string() for w_arg in w_args.fetch_all(self.space)]
                    if len(arg_strings) > 0:
                        print "== Arguments: %s" % ', '.join(arg_strings)
        print "== Smalltalk Stack:%s" % self.print_stack()
        if selector == "":
            selector = self.w_method().lookup_selector
        raise error.Exit("Unhandled %s in headless mode." % selector)


class __extend__(ContextPartShadow):
    # Extensions for --- BlockContextShadow ---

    # === Initialization ===

    @staticmethod
    def build_block_context(space, s_home, argcnt, pc):
        size = s_home.own_size() - s_home.tempsize()
        w_self = W_PointersObject(space, space.w_BlockContext, size)

        ctx = ContextPartShadow(space, w_self, size, space.w_BlockContext)
        ctx.store_expected_argument_count(argcnt)
        ctx.store_w_home(s_home.w_self())
        ctx.store_initialip(pc)
        ctx.store_pc(pc)
        w_self._set_strategy(ctx)
        ctx.init_temps_and_stack()
        return ctx

    # === Implemented accessors ===

    def s_home_block_context(self):
        return self.get_extra_data()._w_home.as_context_get_shadow(self.space)

    def stackstart_block_context(self):
        return constants.BLKCTX_STACK_START

    def tempsize_block_context(self):
        # A blockcontext doesn't have any temps
        return 0

    def w_receiver_block_context(self):
        return self.s_home().w_receiver()

    def w_method_block_context(self):
        retval = self.s_home().w_method()
        assert isinstance(retval, W_CompiledMethod), "block context method is no W_CompiledMethod"
        return retval

    def is_closure_context_block_context(self):
        return True

    def home_is_self_block_context(self):
        return False

    # === Temporary variables ===

    def gettemp_block_context(self, index):
        return self.s_home().gettemp(index)

    def settemp_block_context(self, index, w_value):
        self.s_home().settemp(index, w_value)

    # === Accessing object fields ===

    def fetch_block_context(self, n0):
        if n0 == constants.BLKCTX_HOME_INDEX:
            return self.get_extra_data()._w_home
        if n0 == constants.BLKCTX_INITIAL_IP_INDEX:
            return self.wrap_initialip()
        if n0 == constants.BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX:
            return self.wrap_eargc()
        else:
            return self.fetch_context_part(n0)

    def store_block_context(self, n0, w_value):
        if n0 == constants.BLKCTX_HOME_INDEX:
            return self.store_w_home(w_value)
        if n0 == constants.BLKCTX_INITIAL_IP_INDEX:
            return self.unwrap_store_initialip(w_value)
        if n0 == constants.BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX:
            return self.unwrap_store_eargc(w_value)
        else:
            return self.store_context_part(n0, w_value)

    def store_w_home(self, w_home):
        assert isinstance(w_home, W_PointersObject), "trying to store non-pointer w_home"
        self.get_extra_data()._w_home = w_home

    def unwrap_store_initialip(self, w_value):
        initialip = self.space.unwrap_int(w_value)
        initialip -= 1 + self.w_method().literalsize
        self.store_initialip(initialip)

    def store_initialip(self, initialip):
        self.get_extra_data()._initialip = initialip

    def wrap_initialip(self):
        initialip = self.initialip()
        initialip += 1 + self.w_method().literalsize
        return self.space.wrap_smallint_unsafe(initialip)

    def reset_pc(self):
        self.store_pc(self.initialip())

    def initialip(self):
        return self.get_extra_data()._initialip

    def unwrap_store_eargc(self, w_value):
        self.store_expected_argument_count(self.space.unwrap_int(w_value))

    def wrap_eargc(self):
        return self.space.wrap_smallint_unsafe(self.expected_argument_count())

    def expected_argument_count(self):
        return self.get_extra_data()._eargc

    def store_expected_argument_count(self, argc):
        self.get_extra_data()._eargc = argc

    # === Stack Manipulation ===

    def reset_stack(self):
        self.pop_n(self.stackdepth())

    # === Printing ===

    def w_arguments_block_context(self):
        return []

    def method_str_block_context(self):
        return '[] in %s' % self.w_method().get_identifier_string()


class __extend__(ContextPartShadow):
    # Extensions for --- MethodContextShadow ---

    # === Initialization ===

    @staticmethod
    def build_method_context(space, w_method, w_receiver, arguments=[],
                             closure=None, s_fallback=None):
        w_method = jit.promote(w_method)
        size = w_method.frame_size() + constants.MTHDCTX_TEMP_FRAME_START

        ctx = ContextPartShadow(space, None, size, space.w_MethodContext)
        if s_fallback is not None:
            ctx.get_extra_data()._s_fallback = s_fallback
        ctx.store_w_receiver(w_receiver)
        ctx.store_w_method(w_method)
        ctx.closure = closure
        ctx.init_temps_and_stack()
        ctx.initialize_temps(space, arguments)
        return ctx

    @jit.unroll_safe
    def initialize_temps(self, space, arguments):
        argc = len(arguments)
        for i0 in range(argc):
            w_arg = arguments[i0]
            if isinstance(w_arg, W_BlockClosure) and self.blockmethod is None:
                # If our first argument is a block, we use its home method to
                # distinguish the context from others that receive a different
                # block argument to the same method (e.g. so that all the
                # collection methods are specialized based on where the block
                # that was passed in came from
                self.blockmethod = w_arg.w_method()
            self.settemp(i0, w_arg)
        closure = self.closure
        if closure:
            startpc = jit.promote(closure.startpc())
            pc = startpc - self.w_method().bytecodeoffset() - 1
            self.store_pc(pc)
            for i0 in range(closure.varsize()):
                w_obj = closure.at0(space, i0)
                if isinstance(w_obj, W_BlockClosure) and self.blockmethod is None:
                    # for some methods (see Collection>>do: or detect:ifNone:)
                    # the user code passes in a block, and the block closure
                    # captures that block and is then called in a loop. So if we
                    # activate the block passed in from detect:ifNone: in the
                    # loop, we really want to specialize on the first temp of
                    # that block, which represents the block passed in from user
                    # code into detect:ifNone:
                    self.blockmethod = w_obj.w_method()
                self.settemp(i0+argc, w_obj)

    # === Accessing object fields ===

    def fetch_method_context(self, n0):
        if n0 == constants.MTHDCTX_METHOD:
            return self.w_method()
        if n0 == constants.MTHDCTX_CLOSURE_OR_NIL:
            if self.closure:
                return self.closure
            else:
                return self.space.w_nil
        if n0 == constants.MTHDCTX_RECEIVER:
            return self.w_receiver()
        temp_i = n0-constants.MTHDCTX_TEMP_FRAME_START
        if (0 <= temp_i < self.tempsize()):
            return self.gettemp(temp_i)
        else:
            return self.fetch_context_part(n0)

    def store_method_context(self, n0, w_value):
        if n0 == constants.MTHDCTX_METHOD:
            return self.store_w_method(w_value)
        if n0 == constants.MTHDCTX_CLOSURE_OR_NIL:
            if w_value.is_nil(self.space):
                self.closure = None
            else:
                if not isinstance(w_value, W_BlockClosure):
                    raise error.PrimitiveFailedError
                self.closure = w_value
            return
        if n0 == constants.MTHDCTX_RECEIVER:
            self.store_w_receiver(w_value)
            return
        temp_i = n0-constants.MTHDCTX_TEMP_FRAME_START
        if (0 <= temp_i < self.tempsize()):
            return self.settemp(temp_i, w_value)
        else:
            return self.store_context_part(n0, w_value)

    def store_w_receiver(self, w_receiver):
        self._w_receiver = w_receiver

    # === Implemented Accessors ===

    def s_home_method_context(self):
        if self.is_closure_context():
            # this is a context for a blockClosure
            w_outerContext = self.closure.w_outerContext()
            assert isinstance(w_outerContext, W_PointersObject), "outer context is no pointers object"
            s_outerContext = w_outerContext.as_context_get_shadow(self.space)
            # XXX check whether we can actually return from that context
            if s_outerContext.is_returned():
                raise error.BlockCannotReturnError()
            return s_outerContext.s_home()
        else:
            return self

    def stackstart_method_context(self):
        return constants.MTHDCTX_TEMP_FRAME_START

    def store_w_method(self, w_method):
        assert isinstance(w_method, W_CompiledMethod), "trying to store method that is not a W_CompiledMethod"
        self._w_method = w_method

    def w_receiver_method_context(self):
        return self._w_receiver

    def w_method_method_context(self):
        retval = self._w_method
        assert isinstance(retval, W_CompiledMethod), "method context method is not a W_CompiledMethod"
        return retval

    def tempsize_method_context(self):
        if not self.is_closure_context():
            return self.w_method().tempsize()
        else:
            return self.closure.tempsize()

    def is_closure_context_method_context(self):
        return self.closure is not None

    def home_is_self_method_context(self):
        return not self.is_closure_context()

    # ______________________________________________________________________
    # Marriage of MethodContextShadows with PointerObjects only when required

    def w_self(self):
        if self._w_self is not None:
            return self._w_self
        else:
            space = self.space
            w_self = W_PointersObject(space, space.w_MethodContext, self._w_self_size)
            w_self._set_strategy(self)
            self._w_self = w_self
            return w_self

    # === Temporary variables ===

    def gettemp_method_context(self, index0):
        return self.stack_get(index0)

    def settemp_method_context(self, index0, w_value):
        self.stack_put(index0, w_value)

    # === Printing ===

    def w_arguments_method_context(self):
        argcount = self.w_method().argsize
        return [self.stack_get(i) for i in range(argcount)]

    def method_str_method_context(self):
        block = '[] in ' if self.is_closure_context() else ''
        return '%s%s' % (block, self.w_method().get_identifier_string())
