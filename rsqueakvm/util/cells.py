from rpython.rlib import jit
from rpython.rlib.objectmodel import specialize, import_from_mixin

from rsqueakvm.util.version import Version


class QuasiConstantMixin(object):
    """Mixin for constant values that can be edited, but will be promoted
    to a constant when jitting."""
    _immutable_fields_ = ["value?"]
    def __init__(self, initial_value):
        self.value = initial_value
    def set(self, value):
        self.value = value
    def get(self):
        return jit.promote(self.value)
    def is_set(self):
        return self.get()
    def activate(self):
        self.set(True)
    def deactivate(self):
        self.set(False)
    def changed(self):
        assert isinstance(self.get(), Version)
        self.set(Version())


@specialize.arg(1)
@specialize.argtype(0)
def QuasiConstant(initial_value, type=object):
    class NewQuasiConst(object):
        import_from_mixin(QuasiConstantMixin)
        def _freeze_(self): return True
    return NewQuasiConst(initial_value)
