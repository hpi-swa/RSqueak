from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.util.version import VersionMixin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask


pc_mask     = r_uint(0b11111111110000000000000000000000)
stack_mask  = r_uint(0b11110000111111111111111111111111)
args_mask   = r_uint(0b00001111111111111111111111111111)
pc_shift    = 0
stack_shift = 24
args_shift  = 28


class W_BlockClosure(W_AbstractObjectWithIdentityHash):
    repr_classname = "W_BlockClosure"
    bytes_per_slot = 1
    _attrs_ = [
        "version",
        "_w_outerContext",
        "_startpc_stacklen_args",
        "_stack",
        "_w_method",
        "_w_receiver" ]
    _immutable_fields_ = [
        "version?",
        "_w_outerContext",
        "_startpc_stacklen_args",
        "_stack",
        "_w_method",
        "_w_receiver" ]
    objectmodel.import_from_mixin(VersionMixin)

    def pointers_become_one_way(self, space, from_w, to_w):
        W_AbstractObjectWithIdentityHash.pointers_become_one_way(self, space, from_w, to_w)
        ptrs = self.fetch_all(space)
        ptridx = 0
        for i, w_from in enumerate(from_w):
            try:
                ptridx = ptrs.index(w_from)
            except ValueError:
                continue
            w_to = to_w[i]
            ptrs[ptridx] = w_to
            w_from.post_become_one_way(w_to)
        self.store_all(space, ptrs)

    empty_stack = []

    @jit.unroll_safe
    def __init__(self, space, w_outerctxt, startpc, numArgs, size, stack=None):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._startpc_stacklen_args = r_uint(0)
        self._w_outerContext = w_outerctxt
        self.set_startpc(startpc)
        self.set_numArgs(numArgs)
        if stack:
            self.set_stack(stack)
        elif size == 0:
            self.set_stack(self.empty_stack)
        else:
            self.set_stack([space.w_nil] * size)
        self._fillin_w_method(space)
        self._fillin_w_receiver(space)

    def trace_pointers(self, space):
        ptrs = W_AbstractObjectWithIdentityHash.trace_pointers(self, space)
        return ptrs + self.fetch_all(space)

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        self._startpc_stacklen_args = r_uint(0)
        self.set_stack([space.w_nil] * len(g_self.pointers))
        for i, g_obj in enumerate(g_self.pointers):
            g_obj.fillin(space)
            if i >= constants.BLKCLSR_SIZE:
                self.atput0(space, i - constants.BLKCLSR_SIZE, g_obj.w_object)
            elif i == constants.BLKCLSR_OUTER_CONTEXT:
                self._w_outerContext = g_obj.w_object
            elif i == constants.BLKCLSR_STARTPC:
                self.set_startpc(space.unwrap_int(g_obj.w_object))
            elif i == constants.BLKCLSR_NUMARGS:
                self.set_numArgs(space.unwrap_int(g_obj.w_object))
            else:
                assert False

    def fillin_finalize(self, space, g_self):
        self._fillin_w_method(space)
        self._fillin_w_receiver(space)

    @objectmodel.always_inline
    def _fillin_w_method(self, space):
        self._w_method = self._w_outerContext.fetch(space, constants.MTHDCTX_METHOD)

    @objectmodel.always_inline
    def _fillin_w_receiver(self, space):
        self._w_receiver = self._w_outerContext.fetch(space, constants.MTHDCTX_RECEIVER)

    def getclass(self, space):
        return space.w_BlockClosure

    def instsize(self):
        return constants.BLKCLSR_SIZE

    def varsize(self):
        return self.get_stacksize()

    def size(self):
        return self.instsize() + self.varsize()

    def tempsize(self):
        # We ignore the number of temps a block has, because the first
        # bytecodes of the block will initialize them for us. We will only
        # use this information for deciding where the stack pointer should be
        # initialy.
        # For a finding the correct number, see BlockClosure>#numTemps in an Image.
        return self.varsize() + self.numArgs()

    def fetch(self, space, index0):
        if index0 == constants.BLKCLSR_OUTER_CONTEXT:
            return self.w_outerContext()
        elif index0 == constants.BLKCLSR_STARTPC:
            return space.wrap_int(self.startpc())
        elif index0 == constants.BLKCLSR_NUMARGS:
            return space.wrap_int(self.numArgs())
        else:
            return self.at0(space, index0 - constants.BLKCLSR_SIZE)

    def at0(self, space, index0):
        return self._stack[index0]

    def w_outerContext(self):
        return self._w_outerContext

    def get_stacksize(self):
        return intmask((jit.promote(self._startpc_stacklen_args) & ~stack_mask) >> stack_shift)

    def set_stack(self, lst):
        self._stack = lst
        ln = len(lst)
        self._startpc_stacklen_args = (self._startpc_stacklen_args & stack_mask) | (r_uint(ln) << stack_shift)

    def startpc(self):
        return intmask((jit.promote(self._startpc_stacklen_args) & ~pc_mask) >> pc_shift)

    def set_startpc(self, pc):
        self._startpc_stacklen_args = (self._startpc_stacklen_args & pc_mask) | (r_uint(pc) << pc_shift)

    def numArgs(self):
        return intmask((jit.promote(self._startpc_stacklen_args) & ~args_mask) >> args_shift)

    def set_numArgs(self, numArgs):
        self._startpc_stacklen_args = (self._startpc_stacklen_args & args_mask) | (r_uint(numArgs) << args_shift)

    def w_method(self):
        return self._w_method

    def w_receiver(self):
        return self._w_receiver

    def store(self, space, index0, w_value):
        if index0 >= constants.BLKCLSR_SIZE:
            self.atput0(space, index0 - constants.BLKCLSR_SIZE, w_value)
        else:
            self.changed() # aborts trace
            if index0 == constants.BLKCLSR_OUTER_CONTEXT:
                self._w_outerContext = w_value
                self._fillin_w_method(space)
                self._fillin_w_receiver(space)
            elif index0 == constants.BLKCLSR_STARTPC:
                self.set_startpc(space.unwrap_int(w_value))
            elif index0 == constants.BLKCLSR_NUMARGS:
                self.set_numArgs(space.unwrap_int(w_value))
            else:
                assert False

    def atput0(self, space, index0, w_value):
        self._stack[index0] = w_value

    def fetch_all(self, space):
        return [self.w_outerContext(),
                space.wrap_int(self.startpc()),
                space.wrap_int(self.numArgs())] + self._stack

    @jit.unroll_safe
    def store_all(self, space, lst_w):
        for i, w_v in enumerate(lst_w):
            self.store(space, i, w_v)

    def _become(self, w_other):
        if not isinstance(w_other, W_BlockClosure):
            raise error.PrimitiveFailedError
        self.changed() # aborts trace
        self._w_outerContext, w_other._w_outerContext = w_other._w_outerContext, self._w_outerContext
        self._startpc_stacklen_args, w_other._startpc_stacklen_args = w_other._startpc_stacklen_args, self._startpc_stacklen_args
        self._stack, w_other._stack = w_other._stack, self._stack
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def clone(self, space):
        copy = self.__class__(
            space, self.w_outerContext(), self.startpc(), self.numArgs(), self.varsize(), stack=list(self._stack))
        return copy

    def create_frame(self, space, arguments=[]):
        from rsqueakvm import storage_contexts
        w_method = self.w_method()
        assert isinstance(w_method, W_CompiledMethod)
        w_receiver = self.w_receiver()
        return storage_contexts.ContextPartShadow.build_method_context(
            space,
            w_method,
            w_receiver,
            arguments=arguments,
            closure=self
        )
