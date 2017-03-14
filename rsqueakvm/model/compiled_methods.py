from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject

from rpython.rlib import jit
from rpython.rlib.objectmodel import not_rpython, we_are_translated


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
                "lookup_selector", "compiledin_class", "lookup_class", "_frame_size" ]
    _immutable_fields_ = ["_frame_size?"]
    lookup_selector = "<unknown>"
    lookup_class = None

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
            new_w_class.as_class_get_shadow(space).flush_method_caches()
            compiledin_class.as_class_get_shadow(space).flush_method_caches()
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
            new_w_class.as_class_get_shadow(space).flush_method_caches()
            lookup_class.as_class_get_shadow(space).flush_method_caches()

    def __init__(self, space, bytecount=0, header=0):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self.lookup_selector = "unknown%d" % self.gethash()
        self.bytes = ["\x00"] * bytecount
        self._frame_size = 0
        self.setheader(space, header, initializing=True)
        self.post_init()

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        self.lookup_selector = "unknown%d" % self.gethash()
        self.bytes = [] # make sure the attribute is defined
        # Implicitly sets the header, including self.literalsize
        for i, w_object in enumerate(g_self.get_pointers()):
            self.literalatput0(space, i, w_object, initializing=True)
        self.setbytes(g_self.get_bytes()[self.bytecodeoffset():])
        self.post_init()

    def post_init(self):
        # A hook, usually left empty, but patched e.g. in profiler_plugin
        pass

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

    @not_rpython # Only for testing, not safe.
    def setliterals(self, literals):
        self.literals = literals
        self.compiledin_class = None

    def set_lookup_class_and_name(self, w_class, selector):
        self.lookup_class = w_class
        self.lookup_selector = selector

    def setbytes(self, bytes):
        self.bytes = bytes
        self._frame_size = 0

    def setchar(self, index0, character):
        assert index0 >= 0
        self.bytes[index0] = character
        self._frame_size = 0

    # === Getters ===

    def getclass(self, space):
        return space.w_CompiledMethod

    def getbytes(self):
        return self.bytes

    @jit.elidable_promote()
    def size(self):
        return self.headersize() + self.getliteralsize() + len(self.bytes)

    @jit.elidable_promote()
    def tempsize(self):
        return self._tempsize

    @jit.elidable_promote()
    def getliteralsize(self):
        return self.literalsize * constants.BYTES_PER_WORD

    @jit.elidable_promote()
    def bytecodeoffset(self):
        return self.getliteralsize() + self.headersize()

    def headersize(self):
        return constants.BYTES_PER_WORD

    @jit.elidable_promote()
    def getheader(self):
        return self.header

    @jit.elidable_promote()
    def getliteral(self, index):
        return self.literals[index]

    @jit.elidable_promote()
    def primitive(self):
        return self._primitive

    @jit.elidable_promote()
    def frame_size(self):
        return self._frame_size

    def update_frame_size(self, size):
        self._frame_size = max(self._frame_size, size)

    @jit.elidable_promote()
    def squeak_frame_size(self):
        # From blue book: normal mc have place for 12 temps+maxstack
        # mc for methods with islarge flag turned on 32
        return 16 + self.islarge * 40 + self.argsize

    @jit.elidable_promote()
    def end_pc(self):
        from rsqueakvm.primitives import constants
        from rsqueakvm.interpreter_bytecodes import RETURN_BYTECODES
        primitive = self.primitive()
        if primitive >=  256 and primitive <= 519:
            return self.bytecodeoffset()
        end_pc = 0
        for pos, byte in enumerate(self.bytes):
            if ord(byte) in RETURN_BYTECODES:
                end_pc = pos
        return end_pc

    @jit.elidable_promote()
    def fetch_bytecode(self, pc):
        try:
            return self.bytes[pc]
        except IndexError:
            return chr(120) # returnReceiverBytecode

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

    @jit.elidable_promote()
    def constant_compiledin_class(self):
        return self.compiledin_class

    @jit.elidable_promote()
    def constant_lookup_class(self):
        return self.lookup_class

    def safe_compiled_in(self):
        return self.constant_compiledin_class() or self.constant_lookup_class()

    # === Object Access ===

    @jit.elidable_promote()
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

    @jit.elidable_promote()
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
            if isinstance(w_literal, W_PointersObject):
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

    def _become(self, w_other):
        if not isinstance(w_other, W_CompiledMethod):
            raise error.PrimitiveFailedError
        self.argsize, w_other.argsize = w_other.argsize, self.argsize
        self._primitive, w_other._primitive = w_other._primitive, self._primitive
        self.literals, w_other.literals = w_other.literals, self.literals
        self._tempsize, w_other._tempsize = w_other._tempsize, self._tempsize
        self._frame_size, w_other._frame_size = w_other._frame_size, self._frame_size
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.header, w_other.header = w_other.header, self.header
        self.literalsize, w_other.literalsize = w_other.literalsize, self.literalsize
        self.islarge, w_other.islarge = w_other.islarge, self.islarge
        self.lookup_selector, w_other.lookup_selector = w_other.lookup_selector, self.lookup_selector
        self.compiledin_class, w_other.compiledin_class = w_other.compiledin_class, self.compiledin_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)
        if isinstance(self.compiledin_class, W_PointersObject):
            space = self.compiledin_class.space()
            self.compiledin_class.as_class_get_shadow(space).flush_method_caches()
        if isinstance(w_other.compiledin_class, W_PointersObject):
            space = w_other.compiledin_class.space()
            w_other.compiledin_class.as_class_get_shadow(space).flush_method_caches()

    def clone(self, space):
        copy = self.__class__(space, 0, self.getheader())
        copy.bytes = list(self.bytes)
        copy._frame_size = self._frame_size
        copy.literals = list(self.literals)
        copy.compiledin_class = self.compiledin_class
        copy.lookup_selector = self.lookup_selector
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
        j = 0
        while j < len(self.bytes):
            i = self.bytes[idx]
            retval += '\n'
            retval += '->' if j + 1 is markBytecode else '  '
            retval += ('%0.2i: 0x%0.2x(%0.3i) ' % (j + 1, ord(i), ord(i))) + BYTECODE_TABLE[ord(i)].__name__
            j += 1
        retval += "\n---------------------"
        return retval

    def as_string(self, markBytecode=0):
        retval = "\nMethodname: " + self.get_identifier_string()
        retval += "\n%s" % self.bytecode_string(markBytecode)
        return retval

    def guess_containing_classname(self):
        w_class = self.compiled_in()
        if w_class:
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
        self._frame_size = self.argsize + self._tempsize

    def setbytes(self, bytes):
        W_CompiledMethod.setbytes(self, bytes)
        if SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def setchar(self, index0, character):
        W_CompiledMethod.setchar(self, index0, character)
        if index0 in (1, 2) and SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def update_primitive_index(self):
        if self.bytes[0] == chr(139): # we have a primitive call
            self._primitive = ord(self.bytes[1]) + (ord(self.bytes[2]) << 8)
        else:
            self._primitive = 0

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
        self._frame_size = self.argsize + self._tempsize
