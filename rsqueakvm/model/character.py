from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.numeric import W_SmallInteger


class W_Character(W_AbstractObjectWithIdentityHash):
    """Boxed char value."""
    _attrs_ = ['value']
    repr_classname = "W_Character"

    def __init__(self, value):
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        pointers_w = g_self.pointers
        assert len(pointers_w) == 1
        pointers_w[0].fillin(space)
        self.value = space.unwrap_int(pointers_w[0].w_object)

    def getclass(self, space):
        """Return Character from special objects array."""
        return space.w_Character

    def guess_classname(self):
        return "Character"

    def str_content(self):
        try:
            return "$" + chr(self.value)
        except ValueError:
            return "Character value: " + str(self.value)

    def gethash(self):
        return self.value

    def invariant(self):
        return isinstance(self.value, int)

    def _become(self, w_other):
        assert isinstance(w_other, W_Character)
        self.value, w_other.value = w_other.value, self.value
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def is_same_object(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

    def __eq__(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.value)

    def clone(self, space):
        return self

    def unwrap_char_as_byte(self, space):
        return chr(self.value)

    def at0(self, space, index0):
        return self.fetch(space, index0)

    def atput0(self, space, index0, w_value):
        self.store(space, index0, w_value)

    def fetch(self, space, n0):
        if n0 != 0:
            raise IndexError
        return space.wrap_smallint_unsafe(self.value)

    def store(self, space, n0, w_obj):
        if n0 != 0:
            raise IndexError
        if isinstance(w_obj, W_SmallInteger):
            self.value = w_obj.value
        else:
            raise IndexError

    def size(self):
        return 1
