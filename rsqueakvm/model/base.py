import sys

from rsqueakvm import constants, error

from rpython.rlib import jit, rrandom, signature, objectmodel
from rpython.rlib.rarithmetic import intmask


class W_Object(object):
    """Root of Squeak model, abstract."""
    _attrs_ = []    # no RPython-level instance variables allowed in W_Object
    _settled_ = True
    repr_classname = "W_Object"
    bytes_per_slot = constants.BYTES_PER_WORD

    def size(self):
        """Return the number of "slots" or "items" in the receiver object.
        This means different things for different objects.
        For ByteObject, this means the number of bytes, for WordObject the number of words,
        for PointerObject the number of pointers (regardless if it's varsized or not).
        """
        return 0

    def instsize(self):
        """Return the number of slots of the object reserved for instance variables (not number of bytes).
        Only returns something non-zero for W_PointersObjects,
        because all other classes in this model hierarchy represent varsized classes (except for SmallInteger)."""
        return 0

    def varsize(self):
        """Return number of slots in the of variable-sized part (not number of bytes).
        Not necessarily number of bytes.
        Variable sized objects are those created with #new:."""
        return self.size() - self.instsize()

    def bytesize(self):
        """Return bytesize that conforms to Blue Book.

        The reported size may differ from the actual size in Spy's object
        space, as memory representation varies depending on PyPy translation."""
        return self.size() * self.bytes_per_slot

    def safe_getclass(self, space):
        return self.getclass(space)

    def getclass(self, space):
        """Return Squeak class."""
        raise NotImplementedError()

    def change_class(self, space, w_class):
        raise NotImplementedError()

    def gethash(self):
        """Return 31-bit hash value."""
        raise NotImplementedError()

    def at0(self, space, index0):
        """Access variable-sized part, as by Object>>at:.

        Return value depends on layout of instance. Byte objects return bytes,
        word objects return words, pointer objects return pointers. Compiled method are
        treated special, if index0 within the literalsize returns pointer to literal,
        otherwise returns byte (ie byte code indexing starts at literalsize)."""
        raise NotImplementedError()

    def atput0(self, space, index0, w_value):
        """Access variable-sized part, as by Object>>at:put:.

        Semantics depend on layout of instance. Byte objects set bytes,
        word objects set words, pointer objects set pointers. Compiled method are
        treated special, if index0 within the literalsize sets pointer to literal,
        otherwise patches bytecode (ie byte code indexing starts at literalsize)."""
        raise NotImplementedError()

    def fetch(self, space, n0):
        """Access fixed-size part, maybe also variable-sized part (we have to
        consult the Blue Book)."""
        # TODO check the Blue Book
        raise NotImplementedError()

    def store(self, space, n0, w_value):
        """Access fixed-size part, maybe also variable-sized part (we have to
        consult the Blue Book)."""
        raise NotImplementedError()

    def fillin(self, space, g_self):
        raise NotImplementedError()

    def fillin_weak(self, space, g_self):
        raise NotImplementedError()

    def fillin_finalize(self, space, g_self):
        pass

    def getword(self, n0):
        raise NotImplementedError()

    def setword(self, n0, r_uint_value):
        raise NotImplementedError()

    def invariant(self):
        return True

    def class_shadow(self, space):
        """Return internal representation of Squeak class."""
        w_class = jit.promote(self.getclass(space))
        return w_class.as_class_get_shadow(space)

    def is_same_object(self, other):
        """Compare object identity. This should be used instead of directly
        using is everywhere in the interpreter, in case we ever want to
        implement it differently (which is useful e.g. for proxies). Also,
        SmallIntegers and Floats need a different implementation."""
        return self is other

    def is_nil(self, space):
        """Return True, if the receiver represents the nil object in the given Object Space."""
        return self.is_same_object(space.w_nil)

    def is_class(self, space):
        """ Return true, if the receiver seems to be a class.
        We can not be completely sure about this (non-class objects might be
        used as class)."""
        return False

    def become(self, other):
        """Become swaps two objects.
           False means swapping failed"""
        return False

    def pointers_become_one_way(self, space, from_w, to_w):
        pass

    def post_become_one_way(self, w_to):
        pass

    def clone(self, space):
        raise NotImplementedError

    def has_class(self):
        """All Smalltalk objects should have classes. Unfortuantely for
        bootstrapping the metaclass-cycle and during testing, that is not
        true for some W_PointersObjects"""
        return True

    def classname(self, space):
        """Get the name of the class of the receiver"""
        name = None
        if self.has_class():
            name = self.class_shadow(space).getname()
        if not name:
            name = "?"
        return name

    def is_positive(self, space):
        raise error.UnwrappingError("Got unexpected class in is_positive")

    def unwrap_int(self, space):
        raise error.UnwrappingError("Got unexpected class in unwrap_int")

    def unwrap_uint(self, space):
        raise error.UnwrappingError("Got unexpected class in unwrap_uint")

    def unwrap_int64(self, space):
        raise error.UnwrappingError("Got unexpected class unwrap_int64")

    def unwrap_rbigint(self, space):
        raise error.UnwrappingError("Got unexpected class unwrap_rbigint")

    def unwrap_char_as_byte(self, space):
        raise error.UnwrappingError

    def unwrap_array(self, space):
        raise error.UnwrappingError

    def unwrap_float(self, space):
        raise error.UnwrappingError

    def is_array_object(self):
        return False

    def unwrap_string(self, space):
        raise error.UnwrappingError

    # Methods for printing this object

    def guess_classname(self):
        """Get the name of the class of the receiver without using a space.
        If the shadow of the class of the receiver is not yet initialized,
        this might not return a correct name."""
        return "?"

    def __repr__(self):
        return self.as_repr_string()

    def __str__(self):
        content = self.str_content()
        if content:
            return "a %s(%s)" % (self.guess_classname(), content)
        else:
            return "a %s" % (self.guess_classname())

    def str_content(self):
        return ""

    def as_repr_string(self):
        return "<%s (a %s) %s>" % (self.repr_classname, self.guess_classname(),
                                   self.repr_content())

    def repr_content(self):
        return self.str_content()

    def selector_string(self):
        return self.as_repr_string()


def calculate_and_cache(w_object):
    hash = intmask(objectmodel.compute_identity_hash(w_object)) % 2**22 + 1
    w_object.hash = hash
    return hash


class W_AbstractObjectWithIdentityHash(W_Object):
    """Object with explicit hash (ie all except small
    ints and floats)."""
    _attrs_ = ['hash']
    repr_classname = "W_AbstractObjectWithIdentityHash"

    UNASSIGNED_HASH = 0
    hash = UNASSIGNED_HASH  # default value

    def post_become_one_way(self, w_to):
        if isinstance(w_to, W_AbstractObjectWithIdentityHash):
            w_to.hash = self.gethash()

    def fillin(self, space, g_self):
        self.hash = g_self.get_hash()

    def setchar(self, n0, character):
        raise NotImplementedError()

    def gethash(self):
        return jit.conditional_call_elidable(self.hash, calculate_and_cache, self)

    @objectmodel.always_inline
    def rehash(self):
        self.hash = intmask(objectmodel.compute_identity_hash(self)) % 2**22 + 1

    def invariant(self):
        return isinstance(self.hash, int)

    def become(self, w_other):
        if not self.can_become(w_other):
            return False
        if self.is_same_object(w_other):
            return False
        self._become(w_other)
        return True

    def can_become(self, w_other):
        # TODO -- what about become: with a Float and a CompiledMethod etc.?
        # We might be in trouble regarding W_LargeInteger, too.
        return self.__class__ is w_other.__class__

    def _become(self, w_other):
        if not isinstance(w_other, W_AbstractObjectWithIdentityHash):
            raise error.PrimitiveFailedError
        self.hash, w_other.hash = w_other.hash, self.hash


@signature.finishsigs
class W_AbstractObjectWithClassReference(W_AbstractObjectWithIdentityHash):
    """Objects with arbitrary class (ie not CompiledMethod, SmallInteger or
    Float)."""
    _attrs_ = ['w_class']
    _immutable_fields_ = ['w_class?']
    repr_classname = "W_AbstractObjectWithClassReference"
    w_class = None

    def __init__(self, space, w_class):
        W_AbstractObjectWithIdentityHash.__init__(self)
        from rsqueakvm.model.pointers import W_PointersObject
        if w_class is not None:     # it's None only for testing and space generation
            assert isinstance(w_class, W_PointersObject)
            self.w_class = w_class
        else:
            self.w_class = None

    def repr_content(self):
        return 'len=%d %s' % (self.size(), self.str_content())

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # The class data will be initialized lazily, after the initial fillin-sequence is over.
        # Don't construct the ClassShadow here, yet!
        self.w_class = g_self.get_class()

    def safe_getclass(self, space):
        return self.w_class

    def getclass(self, space):
        return jit.promote(self.w_class)

    def guess_classname(self):
        if self.getclass(None).has_space():
            class_shadow = self.class_shadow(self.getclass(None).space())
            return class_shadow.name
        else:
            # We cannot access the class during the initialization sequence.
            return "?? (class not initialized)"

    def change_class(self, space, w_class):
        self.w_class = w_class

    def invariant(self):
        from rsqueakvm import storage_classes
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.getclass(None).strategy, storage_classes.ClassShadow))

    def pointers_become_one_way(self, space, from_w, to_w):
        from rsqueakvm.model.pointers import W_PointersObject
        W_AbstractObjectWithIdentityHash.pointers_become_one_way(self, space, from_w, to_w)
        idx = 0
        try:
            idx = from_w.index(self.w_class)
        except ValueError:
            return
        w_class = self.w_class
        new_w_class = to_w[idx]
        assert isinstance(new_w_class, W_PointersObject)
        self.w_class = new_w_class
        w_class.post_become_one_way(new_w_class)

    def _become(self, w_other):
        if not isinstance(w_other, W_AbstractObjectWithClassReference):
            raise error.PrimitiveFailedError
        self.w_class, w_other.w_class = w_other.w_class, self.w_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)
