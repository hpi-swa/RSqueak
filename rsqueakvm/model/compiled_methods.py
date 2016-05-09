from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.version import elidable_for_version, VersionMixin

from rpython.rlib.objectmodel import import_from_mixin, we_are_translated


class CompiledMethodHeader(object):
    def __init__(self, header_word):
        self.primitive_index = 0
        self.has_primitive = False
        self.number_of_literals = 0
        self.number_of_temporaries = 0
        self.number_of_arguments = 0
        self.large_frame = 0


class V3CompiledMethodHeader(CompiledMethodHeader):
    def __init__(self, header_word):
        self.primitive_index, self.number_of_literals, self.large_frame, \
                self.number_of_temporaries, self.number_of_arguments = \
                constants.decode_compiled_method_header(header_word)
        self.has_primitive = self.primitive_index != 0


class SpurCompiledMethodHeader(CompiledMethodHeader):
    def __init__(self, header_word):
        from rsqueakvm.util.bitmanipulation import splitter
        self.number_of_literals, is_optimized_bit, has_primitive_bit, \
                self.large_frame, self.number_of_temporaries, \
                self.number_of_arguments, access_mod, instruction_set_bit = \
                splitter[15, 1, 1, 1, 6, 4, 2, 1](header_word)
        self.has_primitive = has_primitive_bit == 1

    @staticmethod
    def has_primitive_bit_set(header_word):
        return header_word & (1 << 16) != 0


class W_CompiledMethod(W_AbstractObjectWithIdentityHash):
    """My instances are methods suitable for interpretation by the virtual machine.  This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields.

    The current format of a CompiledMethod is as follows:

        header (4 bytes)
        literals (4 bytes each)
        bytecodes  (variable)

    An optional method trailer can be part of the bytecodes part.
    """

    repr_classname = "W_CompiledMethod"
    bytes_per_slot = 1
    _attrs_ = [ "version",
                # Method header
                "header", "_primitive", "literalsize", "islarge", "_tempsize", "argsize",
                # Main method content
                "bytes", "literals",
                # Additional info about the method
                "lookup_selector", "compiledin_class", "lookup_class" ]
    _immutable_fields_ = ["version?"]
    lookup_selector = "<unknown>"
    lookup_class = None
    import_from_mixin(VersionMixin)

    def pointers_become_one_way(self, space, from_w, to_w):
        W_AbstractObjectWithIdentityHash.pointers_become_one_way(self, space, from_w, to_w)
        idx = -1
        try:
            idx = from_w.index(self.compiledin_class)
        except ValueError:
            pass
        if idx >= 0:
            compiledin_class = self.compiledin_class
            new_w_class = to_w[idx]
            assert isinstance(new_w_class, W_PointersObject)
            self.compiledin_class = new_w_class
            compiledin_class.post_become_one_way(new_w_class)
            self.changed()
        idx = -1
        try:
            idx = from_w.index(self.lookup_class)
        except ValueError:
            pass
        if idx >= 0:
            lookup_class = self.lookup_class
            new_w_class = to_w[idx]
            assert isinstance(new_w_class, W_PointersObject)
            self.lookup_class = new_w_class
            lookup_class.post_become_one_way(new_w_class)
            self.changed()

    def __init__(self, space, bytecount=0, header=0):
        self.bytes = ["\x00"] * bytecount
        self.setheader(space, header, initializing=True)

    def fillin(self, space, g_self):
        self.bytes = [] # make sure the attribute is defined
        # Implicitly sets the header, including self.literalsize
        for i, w_object in enumerate(g_self.get_pointers()):
            self.literalatput0(space, i, w_object, initializing=True)
        self.setbytes(g_self.get_bytes()[self.bytecodeoffset():])

    # === Setters ===

    def setheader(self, space, header, initializing=False):
        self.header = header

    def initialize_literals(self, number_of_literals, space,
                            initializing=False):
        if initializing or self.literalsize != number_of_literals:
            # Keep the literals if possible.
            self.literalsize = number_of_literals
            self.literals = [space.w_nil] * self.literalsize

    def setliteral(self, index, w_lit):
        self.literals[index] = w_lit
        if index == len(self.literals):
            self.compiledin_class = None
        self.changed()

    def setliteralvariable(self, space, index, w_value):
        w_assoc = self.getliteral(index)
        from rsqueakvm import wrapper
        association = wrapper.AssociationWrapper(space, w_assoc)
        association.store_value(w_value)
        self.changed()

    def setliterals(self, literals):
        """NOT RPYTHON""" # Only for testing, not safe.
        self.literals = literals
        self.compiledin_class = None
        self.changed()

    def set_lookup_class_and_name(self, w_class, selector):
        self.lookup_class = w_class
        self.lookup_selector = selector
        self.changed()

    def setbytes(self, bytes):
        self.bytes = bytes
        self.changed()

    def setchar(self, index0, character):
        assert index0 >= 0
        self.bytes[index0] = character
        self.changed()

    # === Getters ===

    def getclass(self, space):
        return space.w_CompiledMethod

    def getbytes(self):
        return self.bytes

    @elidable_for_version(0)
    def size(self):
        return self.headersize() + self.getliteralsize() + len(self.bytes)

    @elidable_for_version(0)
    def tempsize(self):
        return self._tempsize

    @elidable_for_version(0)
    def getliteralsize(self):
        return self.literalsize * constants.BYTES_PER_WORD

    @elidable_for_version(0)
    def bytecodeoffset(self):
        return self.getliteralsize() + self.headersize()

    def headersize(self):
        return constants.BYTES_PER_WORD

    @elidable_for_version(0)
    def getheader(self):
        return self.header

    @elidable_for_version(1)
    def getliteral(self, index):
        return self.literals[index]

    @elidable_for_version(2, promote=False)
    def getliteralvariable(self, space, index):
        from rsqueakvm import wrapper
        w_assoc = self.getliteral(index)
        association = wrapper.AssociationWrapper(space, w_assoc)
        return association.value()

    @elidable_for_version(0)
    def primitive(self):
        return self._primitive

    @elidable_for_version(0)
    def compute_frame_size(self):
        # From blue book: normal mc have place for 12 temps+maxstack
        # mc for methods with islarge flag turned on 32
        return 16 + self.islarge * 40 + self.argsize

    @elidable_for_version(1)
    def fetch_bytecode(self, pc):
        assert pc >= 0 and pc < len(self.bytes)
        return self.bytes[pc]

    def compiled_in(self):
        # This method cannot be constant/elidable. Looking up the compiledin-class from
        # the literals must be done lazily because we cannot analyze the literals
        # properly during the fillin-phase.

        # Prefer the information stored in the CompiledMethod literal...
        result = self.constant_lookup_class()
        if not result:
            # ...but fall back to our own information if nothing else available.
            result = self.constant_compiledin_class()
            if not result:
                self.update_compiledin_class_from_literals()
                result = self.constant_compiledin_class()
        assert result is None or isinstance(result, W_PointersObject)
        return result

    @elidable_for_version(0)
    def constant_compiledin_class(self):
        return self.compiledin_class

    @elidable_for_version(0)
    def constant_lookup_class(self):
        return self.lookup_class

    def safe_compiled_in(self):
        return self.constant_compiledin_class() or self.constant_lookup_class()

    # === Object Access ===

    def literalat0(self, space, index0):
        if index0 == 0:
            return space.wrap_int(self.getheader())
        else:
            return self.getliteral(index0 - 1)

    def literalatput0(self, space, index0, w_value, initializing=False):
        if index0 == 0:
            header = space.unwrap_int(w_value)
            self.setheader(space, header, initializing)
        else:
            self.setliteral(index0 - 1, w_value)

    def store(self, space, index0, w_v):
        self.atput0(space, index0, w_v)

    def at0(self, space, index0):
        if index0 < self.bytecodeoffset():
            # XXX: find out what happens if unaligned
            # XXX: Looks like Cog raises an exception in this case
            return self.literalat0(space, index0 / constants.BYTES_PER_WORD)
        else:
            # From blue book:
            # The literal count indicates the size of the
            # CompiledMethod's literal frame.
            # This, in turn, indicates where the
            # CompiledMethod's bytecodes start.
            index0 = index0 - self.bytecodeoffset()
            assert index0 < len(self.bytes)
            return space.wrap_int(ord(self.bytes[index0]))

    def atput0(self, space, index0, w_value):
        if index0 < self.bytecodeoffset():
            if index0 % constants.BYTES_PER_WORD != 0:
                raise error.PrimitiveFailedError("improper store")
            self.literalatput0(space, index0 / constants.BYTES_PER_WORD, w_value)
        else:
            index0 = index0 - self.bytecodeoffset()
            assert index0 < len(self.bytes)
            self.setchar(index0, chr(space.unwrap_int(w_value)))

    # === Misc ===

    def update_compiledin_class_from_literals(self):
        # (Blue book, p 607) Last of the literals is either the containing class
        # or an association with compiledin as a class
        literals = self.literals
        if literals and len(literals) > 0:
            w_literal = literals[-1]
            if isinstance(w_literal, W_PointersObject) and w_literal.has_space():
                space = w_literal.space()  # Not pretty to steal the space from another object.
                compiledin_class = None
                if w_literal.is_class(space):
                    compiledin_class = w_literal
                elif w_literal.size() >= 2:
                    from rsqueakvm import wrapper
                    association = wrapper.AssociationWrapper(space, w_literal)
                    w_literal = association.value()
                    if w_literal.is_class(space):
                        compiledin_class = w_literal
                if compiledin_class:
                    self.compiledin_class = w_literal
                    self.changed()

    def _become(self, w_other):
        assert isinstance(w_other, W_CompiledMethod)
        self.argsize, w_other.argsize = w_other.argsize, self.argsize
        self._primitive, w_other._primitive = w_other._primitive, self._primitive
        self.literals, w_other.literals = w_other.literals, self.literals
        self._tempsize, w_other._tempsize = w_other._tempsize, self._tempsize
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.header, w_other.header = w_other.header, self.header
        self.literalsize, w_other.literalsize = w_other.literalsize, self.literalsize
        self.islarge, w_other.islarge = w_other.islarge, self.islarge
        self.lookup_selector, w_other.lookup_selector = w_other.lookup_selector, self.lookup_selector
        self.compiledin_class, w_other.compiledin_class = w_other.compiledin_class, self.compiledin_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)
        self.changed()
        w_other.changed()

    def clone(self, space):
        copy = self.__class__(space, 0, self.getheader())
        copy.bytes = list(self.bytes)
        copy.literals = list(self.literals)
        copy.compiledin_class = self.compiledin_class
        copy.lookup_selector = self.lookup_selector
        copy.changed()
        return copy

    def invariant(self):
        return (W_Object.invariant(self) and
                hasattr(self, 'literals') and
                self.literals is not None and
                hasattr(self, 'bytes') and
                self.bytes is not None and
                hasattr(self, 'argsize') and
                self.argsize is not None and
                hasattr(self, '_tempsize') and
                self._tempsize is not None and
                hasattr(self, '_primitive') and
                self._primitive is not None)

    def is_array_object(self):
        return True

    def create_frame(self, space, receiver, arguments=[], s_fallback=None):
        from rsqueakvm.storage_contexts import ContextPartShadow
        assert len(arguments) == self.argsize
        return ContextPartShadow.build_method_context(
                space, self, receiver, arguments, s_fallback=s_fallback)

    # === Printing ===

    def guess_classname(self):
        return "CompiledMethod"

    def str_content(self):
        return self.get_identifier_string()

    def bytecode_string(self, markBytecode=0):
        from rsqueakvm.interpreter_bytecodes import BYTECODE_TABLE
        retval = "Bytecode:------------"
        j = 1
        for i in self.bytes:
            retval += '\n'
            retval += '->' if j is markBytecode else '  '
            retval += ('%0.2i: 0x%0.2x(%0.3i) ' % (j, ord(i), ord(i))) + BYTECODE_TABLE[ord(i)].__name__
            j += 1
        retval += "\n---------------------"
        return retval

    def as_string(self, markBytecode=0):
        retval = "\nMethodname: " + self.get_identifier_string()
        retval += "\n%s" % self.bytecode_string(markBytecode)
        return retval

    def guess_containing_classname(self):
        w_class = self.compiled_in()
        if w_class and w_class.has_space():
            # Not pretty to steal the space from another object.
            return w_class.as_class_get_shadow(w_class.space()).getname()
        return "? (no compiledin-info)"

    def get_identifier_string(self):
        return "%s >> #%s" % (self.guess_containing_classname(), self.lookup_selector)

    def safe_identifier_string(self):
        if not we_are_translated():
            return self.get_identifier_string()
        # This has the same functionality as get_identifier_string, but without calling any
        # methods in order to avoid side effects that prevent translation.
        w_class = self.safe_compiled_in()
        if isinstance(w_class, W_PointersObject):
            from rsqueakvm.storage_classes import ClassShadow
            s_class = w_class.strategy
            if isinstance(s_class, ClassShadow):
                return "%s >> #%s" % (s_class.getname(), self.lookup_selector)
        return "#%s" % self.lookup_selector


class W_SpurCompiledMethod(W_CompiledMethod):
    """Handles the specialities of the method header in Spur"""

    def setheader(self, space, header, initializing=False):
        decoded_header = SpurCompiledMethodHeader(header)
        self.header = header
        self.initialize_literals(decoded_header.number_of_literals, space,
                initializing)
        self.argsize = decoded_header.number_of_arguments
        self._tempsize = decoded_header.number_of_temporaries
        self.islarge = decoded_header.large_frame
        self.compiledin_class = None
        if decoded_header.has_primitive and len(self.bytes) >= 3:
            self.update_primitive_index()
        else:
            self._primitive = 0
        self.changed()

    def setbytes(self, bytes):
        W_CompiledMethod.setbytes(self, bytes)
        if SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def setchar(self, index0, character):
        W_CompiledMethod.setchar(self, index0, character)
        if index0 in (1, 2) and SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def update_primitive_index(self):
        assert self.bytes[0] == chr(139)
        self._primitive = ord(self.bytes[1]) + (ord(self.bytes[2]) << 8)


class W_PreSpurCompiledMethod(W_CompiledMethod):

    def setheader(self, space, header, initializing=False):
        decoded_header = V3CompiledMethodHeader(header)
        self.header = header
        self.initialize_literals(
                decoded_header.number_of_literals, space, initializing)
        self.argsize = decoded_header.number_of_arguments
        self._tempsize = decoded_header.number_of_temporaries
        self._primitive = decoded_header.primitive_index
        self.islarge = decoded_header.large_frame
        self.compiledin_class = None
        self.changed()
