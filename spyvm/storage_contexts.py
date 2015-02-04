
from spyvm import model, constants, error, wrapper
from spyvm.storage import AbstractRedirectingShadow
from rpython.tool.pairtype import extendabletype
from rpython.rlib import rarithmetic, jit, objectmodel
import rstrategies as rstrat

@objectmodel.specialize.call_location()
def fresh_virtualizable(x):
    return jit.hint(x, access_directly=True, fresh_virtualizable=True)

class ContextState(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name
InactiveContext = ContextState("InactiveContext")
ActiveContext = ContextState("ActiveContext")
DirtyContext = ContextState("DirtyContext")

class ExtendableStrategyMetaclass(extendabletype, rstrat.StrategyMetaclass):
    pass

class ContextPartShadow(AbstractRedirectingShadow):

    __metaclass__ = ExtendableStrategyMetaclass
    _attrs_ = ['_s_sender',
               '_pc', '_temps_and_stack',
               '_stack_ptr', 'instances_w', 'state',
               # Virtualizable doesn't really work with subclassing,
               # so we have all attributes in the top-level class
               # From BlockContext:
               '_w_home', '_initialip', '_eargc',
               # From MethodContext
               'closure', '_w_receiver', '_w_method', '_is_BlockClosure_ensure',
               # And a switch to tell which is which
               'is_block_context']
    repr_classname = "ContextPartShadow"

    _virtualizable_ = [
        '_s_sender',
        "_pc",
        "_temps_and_stack[*]",
        "_stack_ptr",
        "_w_self",
        "_w_self_size",
        'state',
        '_w_home',
        '_initialip',
        '_eargc',
        'closure',
        '_w_receiver',
        '_w_method',
        '_is_BlockClosure_ensure'
    ]

    _immutable_fields_ = ['is_block_context']

    # ______________________________________________________________________
    # Initialization

    @jit.unroll_safe
    def __init__(self, space, w_self, size, is_block_context=False, w_method=None, closure=None):
        old_shadow = None
        if w_self: old_shadow = w_self.shadow

        self = fresh_virtualizable(self)
        self.is_block_context = is_block_context
        self._s_sender = None
        AbstractRedirectingShadow.__init__(self, space, w_self, size)
        self.instances_w = {}
        self.state = InactiveContext
        self.store_pc(0)
        # From BlockContext
        self._w_home = None
        self._initialip = 0
        self._eargc = 0
        # From MethodContext
        self.closure = None
        self._w_method = None
        self._w_receiver = None
        self._is_BlockClosure_ensure = False


        if old_shadow and not is_block_context:
            if not closure:
                w_closure = old_shadow.fetch(constants.MTHDCTX_CLOSURE_OR_NIL)
                if w_closure is not space.w_nil:
                    closure = wrapper.BlockClosureWrapper(space, w_closure)
            if not w_method and not closure:
                w_method = old_shadow.fetch(constants.MTHDCTX_METHOD)
        # Let's create the stack array
        if is_block_context or closure:
            self.closure = closure
            stacksize = self.stacksize() + self.tempsize()
        elif w_method:
            self.store_w_method(w_method)
            # Magic numbers... Takes care of cases where reflective
            # code writes more than actual stack size
            assert isinstance(w_method, model.W_CompiledMethod)
            stacksize = w_method.compute_frame_size()
        else:
            assert False
        self._temps_and_stack = [space.w_nil] * stacksize

    @jit.unroll_safe
    def pre_copy_from(self, other_shadow):
        # Some fields have to be initialized before the rest, to
        # ensure correct initialization.
        privileged_fields = self.fields_to_copy_first()
        for n0 in privileged_fields:
            self.copy_field_from(n0, other_shadow)

    @jit.unroll_safe
    def post_copy_from(self, other_shadow):
        privileged_fields = self.fields_to_copy_first()
        for n0 in range(self.size()):
            if n0 not in privileged_fields:
                self.copy_field_from(n0, other_shadow)

    @jit.unroll_safe
    def copy_from(self, other_shadow):
        self.pre_copy_from(other_shadow)
        # Now the temp size will be known.
        self.init_stack_ptr()
        self.post_copy_from(other_shadow)

    def fields_to_copy_first(self):
        if self.is_block_context:
            return [ constants.BLKCTX_HOME_INDEX ]
        else:
            return [ constants.MTHDCTX_METHOD, constants.MTHDCTX_CLOSURE_OR_NIL ]

    # ______________________________________________________________________
    # Accessing object fields

    def fetch(self, n0):
        if self.is_block_context:
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
            assert temp_i >= 0
            return self.peek(temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return self.space.w_nil
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")

    def store(self, n0, w_value):
        if self.is_block_context:
            return self.store_block_context(n0, w_value)
        else:
            return self.store_method_context(n0, w_value)

    def store_context_part(self, n0, w_value):
        if n0 == constants.CTXPART_SENDER_INDEX:
            assert isinstance(w_value, model.W_PointersObject)
            if w_value.is_nil(self.space):
                self.store_s_sender(None)
            else:
                self.store_s_sender(w_value.as_context_get_shadow(self.space))
            return
        if n0 == constants.CTXPART_PC_INDEX:
            return self.store_unwrap_pc(w_value)
        if n0 == constants.CTXPART_STACKP_INDEX:
            return self.unwrap_store_stackpointer(w_value)
        if self.stackstart() <= n0 < self.external_stackpointer(): # XXX can be simplified?
            temp_i = self.stackdepth() - (n0-self.stackstart()) - 1
            assert temp_i >= 0
            return self.set_top(w_value, temp_i)
        if self.external_stackpointer() <= n0 < self.stackend():
            return
        else:
            # XXX later should store tail out of known context part as well
            raise error.WrapperException("Index in context out of bounds")

    # === Sender ===

    def store_s_sender(self, s_sender):
        if s_sender is not self._s_sender:
            self._s_sender = s_sender
            # If new sender is None, we are just being marked as returned.
            if s_sender is not None and self.state is ActiveContext:
                self.state = DirtyContext

    def w_sender(self):
        sender = self.s_sender()
        if sender is None:
            return self.space.w_nil
        return sender.w_self()

    def s_sender(self):
        return self._s_sender

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
            # TODO Warn back to user
            assert size >= 0
            self.pop_n(depth - size)
        else:
            for i in range(depth, size):
                self.push(self.space.w_nil)

    def stackdepth(self):
        return rarithmetic.intmask(self._stack_ptr)

    def wrap_stackpointer(self):
        return self.space.wrap_int(self.stackdepth())

    # === Program Counter ===

    def store_unwrap_pc(self, w_pc):
        if w_pc.is_nil(self.space):
            self.store_pc(-1)
        else:
            pc = self.space.unwrap_int(w_pc)
            pc -= self.w_method().bytecodeoffset()
            pc -= 1
            self.store_pc(pc)

    def wrap_pc(self):
        pc = self.pc()
        if pc == -1:
            return self.space.w_nil
        else:
            pc += 1
            pc += self.w_method().bytecodeoffset()
            return self.space.wrap_int(pc)

    def pc(self):
        return self._pc

    def store_pc(self, newpc):
        assert newpc >= -1
        self._pc = newpc

    # === Subclassed accessors ===

    def s_home(self):
        if self.is_block_context:
            return self.s_home_block_context()
        else:
            return self.s_home_method_context()

    def stackstart(self):
        if self.is_block_context:
            return self.stackstart_block_context()
        else:
            return self.stackstart_method_context()

    def w_receiver(self):
        if self.is_block_context:
            return self.w_receiver_block_context()
        else:
            return self.w_receiver_method_context()

    def w_method(self):
        if self.is_block_context:
            return self.w_method_block_context()
        else:
            return self.w_method_method_context()

    def tempsize(self):
        if self.is_block_context:
            return self.tempsize_block_context()
        else:
            return self.tempsize_method_context()

    def is_closure_context(self):
        if self.is_block_context:
            return self.is_closure_context_block_context()
        else:
            return self.is_closure_context_method_context()

    def is_BlockClosure_ensure(self):
        if self.is_block_context:
            return False
        else:
            return self._is_BlockClosure_ensure

    def home_is_self(self):
        if self.is_block_context:
            return self.home_is_self_block_context()
        else:
            return self.home_is_self_method_context()

    # === Other properties of Contexts ===

    def mark_returned(self):
        self.store_pc(-1)
        self.store_s_sender(None)

    def is_returned(self):
        return self.pc() == -1 and self.w_sender().is_nil(self.space)

    def external_stackpointer(self):
        return self.stackdepth() + self.stackstart()

    def stackend(self):
        # XXX this is incorrect when there is subclassing
        return self._w_self_size

    def fetch_next_bytecode(self):
        pc = jit.promote(self._pc)
        assert pc >= 0
        self._pc += 1
        return self.fetch_bytecode(pc)

    def fetch_bytecode(self, pc):
        bytecode = self.w_method().fetch_bytecode(pc)
        return ord(bytecode)

    # ______________________________________________________________________
    # Temporary Variables
    #
    # Every context has it's own stack. BlockContexts share their temps with
    # their home contexts. MethodContexts created from a BlockClosure get their
    # temps copied from the closure upon activation. Changes are not propagated back;
    # this is handled by the compiler by allocating an extra Array for temps.

    def gettemp(self, index):
        if self.is_block_context:
            return self.gettemp_block_context(index)
        else:
            return self.gettemp_method_context(index)

    def settemp(self, index, w_value):
        if self.is_block_context:
            return self.settemp_block_context(index, w_value)
        else:
            return self.settemp_method_context(index, w_value)

    # ______________________________________________________________________
    # Stack Manipulation

    def stacksize(self):
        return self.stackend() - self.stackstart()

    def init_stack_ptr(self):
        tempsize = self.tempsize()
        self._stack_ptr = rarithmetic.r_uint(tempsize) # we point after the last element

    def stack_get(self, index0):
        return self._temps_and_stack[index0]

    def stack_put(self, index0, w_val):
        assert w_val is not None
        self._temps_and_stack[index0] = w_val

    def stack(self):
        """NOT_RPYTHON""" # purely for testing
        return self._temps_and_stack[self.tempsize():self._stack_ptr]

    def pop(self):
        #assert self._stack_ptr > self.tempsize()
        ptr = jit.promote(self._stack_ptr) - 1
        ret = self.stack_get(ptr)   # you get OverflowError if the stack is empty
        self.stack_put(ptr, self.space.w_nil)
        self._stack_ptr = ptr
        return ret

    def push(self, w_v):
        #assert self._stack_ptr >= self.tempsize()
        #assert self._stack_ptr < self.stackend() - self.stackstart() + self.tempsize()
        ptr = jit.promote(self._stack_ptr)
        self.stack_put(ptr, w_v)
        self._stack_ptr = ptr + 1

    @jit.unroll_safe
    def push_all(self, lst):
        for elt in lst:
            self.push(elt)

    def top(self):
        return self.peek(0)

    def set_top(self, value, position=0):
        rpos = rarithmetic.r_uint(position)
        ptr = self._stack_ptr + ~rpos
        self.stack_put(ptr, value)

    def peek(self, idx):
        rpos = rarithmetic.r_uint(idx)
        ptr = jit.promote(self._stack_ptr) + ~rpos
        return self.stack_get(ptr)

    @jit.unroll_safe
    def pop_n(self, n):
        #assert n == 0 or self._stack_ptr - n >= self.tempsize()
        jit.promote(self._stack_ptr)
        while n > 0:
            n -= 1
            self._stack_ptr -= 1
            self.stack_put(self._stack_ptr, self.space.w_nil)

    @jit.unroll_safe
    def pop_and_return_n(self, n):
        result = [self.peek(i) for i in range(n - 1, -1, -1)]
        self.pop_n(n)
        return result

    # ______________________________________________________________________
    # Primitive support

    def store_instances_array(self, w_class, match_w):
        # used for primitives 77 & 78
        self.instances_w[w_class] = match_w

    @jit.elidable
    def instances_array(self, w_class):
        return self.instances_w.get(w_class, None)

    # ______________________________________________________________________
    # Printing

    def w_arguments(self):
        if self.is_block_context:
            return self.w_arguments_block_context()
        else:
            return self.w_arguments_method_context()

    def method_str(self):
        if self.is_block_context:
            return self.method_str_block_context()
        else:
            return self.method_str_method_context()

    def argument_strings(self):
        return [ w_arg.as_repr_string() for w_arg in self.w_arguments() ]

    def __str__(self):
        retval = self.short_str()
        retval += "\n%s" % self.w_method().bytecode_string(markBytecode=self.pc() + 1)
        retval += "\nArgs:----------------"
        argcount = self.w_method().argsize
        j = 0
        for w_obj in self._temps_and_stack[:self._stack_ptr]:
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
        if self.s_sender() is not None:
            padding, ret_str = self.s_sender().print_padded_stack(method)
        if method:
            desc = self.method_str()
        else:
            desc = self.short_str()
        return padding + ' ', '%s\n%s%s' % (ret_str, padding, desc)


class BlockContextMarkerClass(AbstractRedirectingShadow):
    pass


class __extend__(ContextPartShadow):
    # repr_classname = "BlockContextShadow"

    # === Initialization ===

    @staticmethod
    def build_block_context(space, s_home, argcnt, pc):
        size = s_home.size() - s_home.tempsize()
        w_self = model.W_PointersObject(space, space.w_BlockContext, size)

        ctx = ContextPartShadow(space, w_self, size, is_block_context=True)
        ctx.store_expected_argument_count(argcnt)
        ctx.store_w_home(s_home.w_self())
        ctx.store_initialip(pc)
        ctx.store_pc(pc)
        w_self.store_shadow(ctx)
        ctx.init_stack_ptr()
        return ctx

    # === Implemented accessors ===

    def s_home_block_context(self):
        return self._w_home.as_methodcontext_get_shadow(self.space)

    def stackstart_block_context(self):
        return constants.BLKCTX_STACK_START

    def tempsize_block_context(self):
        # A blockcontext doesn't have any temps
        return 0

    def w_receiver_block_context(self):
        return self.s_home().w_receiver()

    def w_method_block_context(self):
        retval = self.s_home().w_method()
        assert isinstance(retval, model.W_CompiledMethod)
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
            return self._w_home
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
        assert isinstance(w_home, model.W_PointersObject)
        self._w_home = w_home

    def unwrap_store_initialip(self, w_value):
        initialip = self.space.unwrap_int(w_value)
        initialip -= 1 + self.w_method().literalsize
        self.store_initialip(initialip)

    def store_initialip(self, initialip):
        self._initialip = initialip

    def wrap_initialip(self):
        initialip = self.initialip()
        initialip += 1 + self.w_method().literalsize
        return self.space.wrap_int(initialip)

    def reset_pc(self):
        self.store_pc(self.initialip())

    def initialip(self):
        return self._initialip

    def unwrap_store_eargc(self, w_value):
        self.store_expected_argument_count(self.space.unwrap_int(w_value))

    def wrap_eargc(self):
        return self.space.wrap_int(self.expected_argument_count())

    def expected_argument_count(self):
        return self._eargc

    def store_expected_argument_count(self, argc):
        self._eargc = argc

    # === Stack Manipulation ===

    def reset_stack(self):
        self.pop_n(self.stackdepth())

    # === Printing ===

    def w_arguments_block_context(self):
        return []

    def method_str_block_context(self):
        return '[] in %s' % self.w_method().get_identifier_string()


class MethodContextMarkerClass(AbstractRedirectingShadow):
    pass


class __extend__(ContextPartShadow):
    # repr_classname = "MethodContextShadow"

    # === Initialization ===

    @staticmethod
    def build_method_context(space, w_method, w_receiver, arguments=[], closure=None):
        s_MethodContext = space.w_MethodContext.as_class_get_shadow(space)
        size = w_method.compute_frame_size() + s_MethodContext.instsize()

        ctx = ContextPartShadow(space, None, size, is_block_context=False, w_method=w_method, closure=closure)
        ctx.store_w_receiver(w_receiver)
        ctx.store_w_method(w_method)
        ctx.closure = closure
        ctx.init_stack_ptr()
        ctx.initialize_temps(arguments)
        return ctx

    @jit.unroll_safe
    def initialize_temps(self, arguments):
        argc = len(arguments)
        for i0 in range(argc):
            self.settemp(i0, arguments[i0])
        closure = self.closure
        if closure:
            pc = closure.startpc() - self.w_method().bytecodeoffset() - 1
            self.store_pc(pc)
            for i0 in range(closure.size()):
                self.settemp(i0+argc, closure.at0(i0))

    # === Accessing object fields ===

    def fetch_method_context(self, n0):
        if n0 == constants.MTHDCTX_METHOD:
            return self.w_method()
        if n0 == constants.MTHDCTX_CLOSURE_OR_NIL:
            if self.closure:
                return self.closure._w_self
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
                self.closure = wrapper.BlockClosureWrapper(self.space, w_value)
            return
        if n0 == constants.MTHDCTX_RECEIVER:
            self.store_w_receiver(w_value)
            return
        temp_i = n0-constants.MTHDCTX_TEMP_FRAME_START
        if (0 <=  temp_i < self.tempsize()):
            return self.settemp(temp_i, w_value)
        else:
            return self.store_context_part(n0, w_value)

    def store_w_receiver(self, w_receiver):
        self._w_receiver = w_receiver

    # === Implemented Accessors ===

    def s_home_method_context(self):
        if self.is_closure_context():
            # this is a context for a blockClosure
            w_outerContext = self.closure.outerContext()
            assert isinstance(w_outerContext, model.W_PointersObject)
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
        assert isinstance(w_method, model.W_CompiledMethod)
        self._w_method = w_method
        # Primitive 198 is a marker used in BlockClosure >> ensure:
        self._is_BlockClosure_ensure = (w_method.primitive() == 198)

    def w_receiver_method_context(self):
        return self._w_receiver

    def w_method_method_context(self):
        retval = self._w_method
        assert isinstance(retval, model.W_CompiledMethod)
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
            w_self = model.W_PointersObject(space, space.w_MethodContext, self.size())
            w_self.store_shadow(self)
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
        return [ self.stack_get(i) for i in range(argcount) ]

    def method_str_method_context(self):
        block = '[] in ' if self.is_closure_context() else ''
        return '%s%s' % (block, self.w_method().get_identifier_string())
