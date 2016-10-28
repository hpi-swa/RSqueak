from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.version import VersionMixin, elidable_for_version

from rpython.rlib import jit
from rpython.rlib.objectmodel import import_from_mixin, we_are_translated

class W_BlockClosure(W_AbstractObjectWithIdentityHash):
    repr_classname = "W_BlockClosure"
    bytes_per_slot = 1
    _attrs_ = [ "_w_outerContext",
                "_startpc",
                "_numArgs", "_stack", "version" ]
    _immutable_attrs_ = ["version?"]
    import_from_mixin(VersionMixin)

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
        self.changed()

    @jit.unroll_safe
    def __init__(self, space, w_outerctxt, startpc, numArgs, size):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._w_outerContext = w_outerctxt
        self._startpc = startpc
        self._numArgs = numArgs
        self._stack = [space.w_nil] * size
        self.changed()

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        self._stack = [space.w_nil] * len(g_self.pointers)
        for i, g_obj in enumerate(g_self.pointers):
            g_obj.fillin(space)
            self.store(space, i, g_obj.w_object)
        self.changed()

    def getclass(self, space):
        return space.w_BlockClosure

    def instsize(self):
        return constants.BLKCLSR_SIZE

    @elidable_for_version(0)
    def varsize(self):
        return len(self._stack)

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

    @elidable_for_version(0)
    def w_outerContext(self):
        return self._w_outerContext

    @elidable_for_version(0)
    def startpc(self):
        return self._startpc

    @elidable_for_version(0)
    def numArgs(self):
        return self._numArgs

    def store(self, space, index0, w_value):
        if index0 >= constants.BLKCLSR_SIZE:
            self.atput0(space, index0 - constants.BLKCLSR_SIZE, w_value)
        else:
            if index0 == constants.BLKCLSR_OUTER_CONTEXT:
                self._w_outerContext = w_value
            elif index0 == constants.BLKCLSR_STARTPC:
                self._startpc = space.unwrap_int(w_value)
            elif index0 == constants.BLKCLSR_NUMARGS:
                self._numArgs = space.unwrap_int(w_value)
            else:
                assert False
            self.changed()

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
        assert isinstance(w_other, W_BlockClosure)
        self._numArgs, w_other._numArgs = w_other._numArgs, self._numArgs
        self._w_outerContext, w_other._w_outerContext = w_other._w_outerContext, self._w_outerContext
        self._startpc, w_other._startpc = w_other._startpc, self._startpc
        self._stack, w_other._stack = w_other._stack, self._stack
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def clone(self, space):
        copy = self.__class__(
            space, self.w_outerContext(), self.startpc(), self.numArgs(), self.varsize())
        copy._stack = list(self._stack)
        return copy

    def create_frame(self, space, w_outerContext, arguments=[]):
        from rsqueakvm import storage_contexts
        s_outerContext = self.w_outerContext().as_context_get_shadow(space)
        assert not s_outerContext.pure_is_block_context()
        w_method = s_outerContext.w_method()
        w_receiver = s_outerContext.w_receiver()
        return storage_contexts.ContextPartShadow.build_method_context(
            space,
            w_method,
            w_receiver,
            arguments=arguments,
            closure=self
        )
