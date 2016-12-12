import os
import time

from rsqueakvm import constants, error, wrapper
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.compiled_methods import W_CompiledMethod, W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_LargeIntegerWord, W_LargeIntegerBig, W_LargeInteger
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.block_closure import W_BlockClosure
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.util import stream, system
from rsqueakvm.util.bitmanipulation import splitter
from rsqueakvm.util.progress import Progress

from rpython.rlib import objectmodel
from rpython.rlib.rarithmetic import r_ulonglong, r_longlong, r_int, intmask, r_uint, r_uint32, r_int64
from rpython.rlib import jit, rbigint, unroll

if r_longlong is not r_int:
    r_uint64 = r_ulonglong
else:
    r_uint64 = r_uint


# Access for module users
Stream = stream.Stream

# ____________________________________________________________
#
# Constants and image versions.

# from the squeak source code:
# in squeak, the compact classes array can be found at this position
# in the special objects array
COMPACT_CLASSES_ARRAY = 28

# The image data can optionally start after this fixed offset.
POSSIBLE_IMAGE_OFFSET = 512

class ImageVersion(object):
    _immutable_fields_ = [
        "magic", "is_big_endian", "is_64bit", "has_closures",
        "has_floats_reversed", "is_modern", "is_spur"]

    def __init__(self, magic, is_big_endian, is_64bit, has_closures,
                 has_floats_reversed, is_spur=False):
        self.magic = magic
        self.is_big_endian = is_big_endian
        self.is_64bit = is_64bit
        self.has_closures = has_closures
        self.has_floats_reversed = has_floats_reversed
        self.is_modern = magic > 6502
        self.is_spur = is_spur

    def configure_stream(self, stream):
        stream.big_endian = self.is_big_endian
        if self.is_64bit:
            if not system.IS_64BIT:
                raise error.FatalError("Cannot handle 64-bit image.")
            stream.be_64bit()
        else:
            stream.be_32bit()

image_versions = {
    0x00001966:         ImageVersion(6502,  True,  False, False, False),
    0x66190000:         ImageVersion(6502,  False, False, False, False),
    0x00001968:         ImageVersion(6504,  True,  False, True,  False),
    0x68190000:         ImageVersion(6504,  False, False, True,  False),
    0x00001969:         ImageVersion(6505,  True,  False, True,  True ),
    0x69190000:         ImageVersion(6505,  False, False, True,  True ),
    0x00001979:         ImageVersion(6521,  True,  False, True,  True , is_spur=True),
    0x79190000:         ImageVersion(6521,  False, False, True,  True , is_spur=True),
    # CUSTOM VERSION MAGIC: These are for a Spur-format image that we have
    # written from an old image with block-contexts
    0x34120000:         ImageVersion(6521,  False,  False, False,  True , is_spur=True)
}

image_versions_64bit = {
    # Versions for 64 bit images (expressed as two 32-bit words)
    (0x00000000,  0x000109A0): ImageVersion(68000, True,  True,  False, False),
    (-0x5ff6ff00, 0x00000000): ImageVersion(68000, False, True,  False, False), # 0xA009010000000000
    (0x00000000,  0x000109A2): ImageVersion(68002, True,  True,  True,  False),
    (-0x5df6ff00, 0x00000000): ImageVersion(68002, False, True,  True,  False), # 0xA209010000000000
    (0x00000000,  0x000109A3): ImageVersion(68003, True,  True,  True,  True ),
    (-0x5cf6ff00, 0x00000000): ImageVersion(68003, False, True,  True,  True ), # 0xA309010000000000
    # TODO: add 64bit Spur once supported
}

# ____________________________________________________________
#
# Parser classes for Squeak image format.

init_g_objects_driver = jit.JitDriver(name="init_g_objects", reds=['chunk'], greens=['self'])
init_w_objects_driver = jit.JitDriver(name="init_w_objects", reds=['chunk'], greens=['self'])
fillin_w_objects_driver = jit.JitDriver(name="fillin_w_objects", reds=['chunk'], greens=['self'])
fillin_weak_w_objects_driver = jit.JitDriver(name="fillin_weak_w_objects", reds=['chunk'], greens=['self'])

def set_reader_user_param(arg):
    jit.set_user_param(init_g_objects_driver, arg)
    jit.set_user_param(init_w_objects_driver, arg)
    jit.set_user_param(fillin_w_objects_driver, arg)
    jit.set_user_param(fillin_weak_w_objects_driver, arg)

# Defaults for reading JIT
set_reader_user_param("threshold=2,function_threshold=2,trace_eagerness=2,loop_longevity=100")


class ImageReader(object):
    _immutable_fields_ = ["space", "stream", "version", "readerStrategy", "logging_enabled"]

    def __init__(self, space, stream, logging_enabled=False):
        self.space = space
        self.stream = stream
        self.version = None
        self.readerStrategy = None
        self.logging_enabled = logging_enabled

    def create_image(self):
        self.read_all()
        return SqueakImage(self)

    def read_all(self):
        self.read_header()
        self.readerStrategy.read_and_initialize()

    def try_read_version(self):
        magic1 = self.stream.next()
        version = image_versions.get(magic1, None)
        if version:
            return version
        # Check 64 bit version
        magic2 = self.stream.next()
        version = image_versions_64bit.get((magic1, magic2), None)
        if not version:
            self.stream.reset()
        return version

    def read_version(self):
        version = self.try_read_version()
        if not version:
            if self.stream.length() > POSSIBLE_IMAGE_OFFSET + 4:
                self.stream.skipbytes(POSSIBLE_IMAGE_OFFSET)
                version = self.try_read_version()
        if not version:
            raise error.CorruptImageError("Illegal version magic.")
        version.configure_stream(self.stream)
        self.version = version
        self.readerStrategy = self.choose_reader_strategy()
        if not version.has_closures:
            self.space.uses_block_contexts.activate()

    def read_header(self):
        self.read_version()
        self.readerStrategy.continue_read_header()
        self.readerStrategy.skip_to_body()

    def choose_reader_strategy(self):
        if self.version.is_spur:
            return SpurReader(self, self.version, self.stream, self.space)
        if self.version.is_modern:
            return NonSpurReader(self, self.version, self.stream, self.space)
        return AncientReader(self, self.version, self.stream, self.space)

    def g_class_of(self, chunk):
        return self.readerStrategy.g_class_of(chunk)

    @property
    def compactclasses(self):
        return self.readerStrategy.compactclasses

    @property
    def intcache(self):
        return self.readerStrategy.intcache

    @property
    def chunklist(self):
        return self.readerStrategy.chunklist

    @property
    def lastWindowSize(self):
        return self.readerStrategy.lastWindowSize

    @property
    def chunks(self):
        return self.readerStrategy.chunks

    def decode_pointers(self, g_object, space, end=-1):
        return self.readerStrategy.decode_pointers(g_object, space, end)

class BaseReaderStrategy(object):
    _immutable_fields_ = ["imageReader", "version", "stream", "space", "chunks", "chunklist"]

    def __init__(self, imageReader, version, stream, space):
        self.imageReader = imageReader
        self.version = version
        self.stream = stream
        self.space = space
        self.chunks = {} # Dictionary mapping old address to chunk object
        self.chunklist = [] # Flat list of all read chunks
        self.intcache = {} # Cached instances of SmallInteger
        self.lastWindowSize = 0
        self._progress = Progress(stages=5, silent=space.silent.is_set())  # Track 5 stages in read_and_initialize

    def log(self, msg):
        if self.imageReader.logging_enabled:
            print msg

    def continue_read_header(self):
        # 1 word headersize
        self.headersize = self.stream.next()
        # 1 word size of the full image
        self.endofmemory = self.stream.next()  # endofmemory = bodysize
        # 1 word old base address
        self.oldbaseaddress = self.stream.next()
        # 1 word pointer to special objects array
        self.specialobjectspointer = self.stream.next()
        # 1 word last used hash
        lasthash = self.stream.next()
        self.lastWindowSize = self.stream.next()
        fullscreenflag = self.stream.next()
        extravmmemory = self.stream.next()

    def skip_to_body(self):
        self.stream.skipbytes(self.headersize - self.stream.pos)

    def read_and_initialize(self):
        self.read_body()
        # All chunks are read, now convert them to real objects.
        self.init_g_objects()
        self.assign_prebuilt_constants()
        self.init_w_objects()
        self.fillin_w_objects()
        self.fillin_weak_w_objects()
        self.fillin_finalize()

    def read_body(self):
        raise NotImplementedError("subclass must override this")

    def init_compactclassesarray(self):
        raise NotImplementedError("subclass must override this")

    def g_class_of(self, chunk):
        raise NotImplementedError("subclass must override this")

    def init_g_objects(self):
        self._progress.next_stage(len(self.chunks))
        for chunk in self.chunks.itervalues():
            self.init_g_object(chunk)
            self._progress.update()
        self.special_g_objects = self.chunks[self.specialobjectspointer].g_object.pointers

    def init_g_object(self, chunk):
        init_g_objects_driver.jit_merge_point(self=self, chunk=chunk)
        chunk.as_g_object(jit.promote(self), self.space)  # initialize g_object

    def assign_prebuilt_constants(self):
        g_special_objects_array = self.chunks[self.specialobjectspointer].g_object
        g_special_objects_array.w_object = self.space.w_special_objects
        # Assign classes and objects that in special objects array that are already created.
        self._assign_prebuilt_constants()

    def smalltalk_g_at(self, lookup_name):
        try:
            g_smalltalk = self.special_g_object(constants.SO_SMALLTALK)
        except IndexError:
            # should be only in tests
            return None
        array_g = []
        if len(g_smalltalk.pointers) == 1:
            # modern image
            globals_g = g_smalltalk.pointers[0].pointers
            if len(globals_g) == 6:
                bindings_g = globals_g[2].pointers
                if len(bindings_g) == 2:
                    array_g = bindings_g[1].pointers
            elif len(globals_g) == 4:
                array_g = globals_g[1].pointers
        elif len(g_smalltalk.pointers) == 2:
            # old image
            array_g = g_smalltalk.pointers[1].pointers
        for g_assoc in array_g:
            if len(g_assoc.pointers) == 2:
                g_name = g_assoc.pointers[0]
                if self.isbytes(g_name):
                    name = "".join(g_name.get_bytes())
                    if name == lookup_name:
                        return g_assoc.pointers[1]
        return None

    def special_g_object(self, index):
        # while python would raise an IndexError, after translation a nonexisting key results in a segfault...
        if index >= len(self.special_g_objects):
            raise IndexError
        return self.special_g_objects[index]

    def special_g_object_safe(self, index):
        # while python would raise an IndexError, after translation a nonexisting key results in a segfault...
        if index >= len(self.special_g_objects):
            return self.special_g_objects[constants.SO_NIL]
        return self.special_g_objects[index]

    def init_w_objects(self):
        self._progress.next_stage(len(self.chunks))
        for g in self.special_g_objects:
            self.init_w_object(g.chunk)
            self._progress.update()
        for chunk in self.chunks.itervalues():
            self.init_w_object(chunk)
            self._progress.update()

    def init_w_object(self, chunk):
        init_w_objects_driver.jit_merge_point(self=self, chunk=chunk)
        chunk.g_object.init_w_object(self.space)

    def fillin_w_objects(self):
        self._progress.next_stage(len(self.chunks))
        for chunk in self.chunks.itervalues():
            self.fillin_w_object(chunk)
            self._progress.update()

    def fillin_w_object(self, chunk):
        fillin_w_objects_driver.jit_merge_point(self=self, chunk=chunk)
        chunk.g_object.fillin(self.space)

    def fillin_weak_w_objects(self):
        self._progress.next_stage(len(self.chunks))
        for chunk in self.chunks.itervalues():
            self.fillin_weak_w_object(chunk)
            self._progress.update()

    def fillin_weak_w_object(self, chunk):
        fillin_weak_w_objects_driver.jit_merge_point(self=self, chunk=chunk)
        chunk.g_object.fillin_weak(self.space)

    def fillin_finalize(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.fillin_finalize(self.space)

    def len_bytes_of(self, chunk):
        return len(chunk.data) * 4

    def get_bytes_of(self, chunk):
        bytes = []
        if self.version.is_big_endian:
            for each in chunk.data:
                bytes.append(chr((each >> 24) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 0) & 0xff))
        else:
            for each in chunk.data:
                bytes.append(chr((each >> 0) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 24) & 0xff))
        return bytes

    def isfloat(self, g_object):
        return self.iswords(g_object) and self.space.w_Float.is_same_object(g_object.g_class.w_object)

    def islargeinteger(self, g_object):
        g_lpi = self.special_g_object_safe(constants.SO_LARGEPOSITIVEINTEGER_CLASS)
        g_lni = self.special_g_object_safe(constants.SO_LARGENEGATIVEINTEGER_CLASS)
        is_large = (g_lpi == g_object.g_class or g_lni == g_object.g_class)
        if is_large:
            assert self.isbytes(g_object)
        return is_large

    def issignedinteger(self, g_object):
        if not self.islargeinteger(g_object):
            return False
        bytes = g_object.get_bytes()
        value = rbigint.rbigint.frombytes(bytes, 'little', False)
        if g_object.g_class != self.special_g_object_safe(constants.SO_LARGEPOSITIVEINTEGER_CLASS):
            value = value.neg()
        try:
            value.toint()
        except OverflowError:
            return False
        return True

    def isunsignedinteger(self, g_object):
        return self.islargeinteger(g_object) and g_object.len_bytes() == constants.BYTES_PER_MACHINE_INT

    def isbiginteger(self, g_object):
        return self.islargeinteger(g_object) and g_object.len_bytes() > constants.BYTES_PER_MACHINE_INT


def make_assign_prebuilt_constants():
    code = ["def _assign_prebuilt_constants(self):"]
    for name, so_index in constants.constant_objects_in_special_object_table_wo_types.items():
        code.extend([
            "",
            "    w_object = self.space.w_%s" % name,
            "    g_object = None",
            "    try:",
            "        g_object = self.special_g_object(%d)" % so_index,
            "    except IndexError:",
        ])
        if name in ("LargeNegativeInteger", "ClassBinding", "Metaclass"):
            code.extend([
                "        g_object = self.smalltalk_g_at('%s')" % name,
            ])
        else:
            code.extend([
                "        pass"
            ])
        code.extend([
            "    if (g_object is not None) and (g_object.w_object is None):",
            "        g_object.w_object = w_object",
            "    elif (g_object is not None) and (not g_object.w_object.is_nil(self.space)):",
            "        raise Warning('Object found in multiple places in the special objects array')"
        ])
    d = {}
    exec compile("\n".join(code), __file__, 'exec') in d
    return d["_assign_prebuilt_constants"]
BaseReaderStrategy._assign_prebuilt_constants = make_assign_prebuilt_constants()


class NonSpurReader(BaseReaderStrategy):

    def read_body(self):
        self.stream.reset_count()
        self._progress.next_stage(self.stream.length())
        while self.stream.count < self.endofmemory:
            chunk, pos = self.read_object()
            self._progress.update(self.stream.count)
            self.chunklist.append(chunk)
            self.chunks[pos + self.oldbaseaddress] = chunk
        self.stream.close()
        return self.chunklist # return for testing

    def init_g_objects(self):
        self.init_compactclassesarray()
        BaseReaderStrategy.init_g_objects(self)

    def read_object(self):
        kind = self.stream.peek() & 3 # 2 bits
        if kind == 0: # 00 bits
            chunk, pos = self.read_3wordobjectheader()
        elif kind == 1: # 01 bits
            chunk, pos = self.read_2wordobjectheader()
        elif kind == 3: # 11 bits
            chunk, pos = self.read_1wordobjectheader()
        else: # 10 bits
            raise error.CorruptImageError("Unused block not allowed in image")
        size = intmask(chunk.size)
        chunk.data = [self.stream.next()
                     for _ in range(size - 1)] #size-1, excluding header
        return chunk, pos

    def read_1wordobjectheader(self):
        kind, size, format, classid, idhash = (
            splitter[2,6,4,5,12](self.stream.next()))
        assert kind == 3
        return ImageChunk(size, format, classid, idhash), self.stream.count - 4

    def read_2wordobjectheader(self):
        assert self.stream.peek() & 3 == 1 #kind
        classid = self.stream.next() - 01 # remove headertype to get pointer
        kind, size, format, _, idhash = splitter[2,6,4,5,12](self.stream.next())
        assert kind == 1
        return ImageChunk(size, format, classid, idhash), self.stream.count - 4

    def read_3wordobjectheader(self):
        kind, size = splitter[2,30](self.stream.next())
        assert kind == 0
        assert splitter[2](self.stream.peek())[0] == 0 #kind
        classid = self.stream.next() - 00 # remove headertype to get pointer
        kind, _, format, _, idhash = splitter[2,6,4,5,12](self.stream.next())
        assert kind == 0
        return ImageChunk(size, format, classid, idhash), self.stream.count - 4

    def init_compactclassesarray(self):
        """ from the blue book (CompiledMethod Symbol Array PseudoContext LargePositiveInteger nil MethodDictionary Association Point Rectangle nil TranslatedMethod BlockContext MethodContext nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil ) """
        special = self.chunks[self.specialobjectspointer]
        assert special.size > 24 #at least
        assert special.format == 2
        # squeak-specific: compact classes array
        chunk = self.chunks[special.data[COMPACT_CLASSES_ARRAY]]
        assert len(chunk.data) == 31
        assert chunk.format == 2
        self.compactclasses = [self.chunks[pointer] for pointer in chunk.data]

    def g_class_of(self, chunk):
        if chunk.iscompact():
            return self.compactclasses[chunk.classid
                - 1].g_object # Smalltalk is 1-based indexed
        else:
            return self.chunks[chunk.classid].g_object

    def decode_pointers(self, g_object, space, end=-1):
        if end == -1:
            end = len(g_object.chunk.data)
        pointers = []
        for i in range(end):
            pointer = g_object.chunk.data[i]
            if (pointer & 1) == 1:
                # pointer = ...1
                # tagged integer
                small_int = GenericObject()
                small_int.initialize_int(pointer >> 1, self, space)
                pointers.append(small_int)
            else:
                # pointer = ...0
                pointers.append(self.chunks[pointer].g_object)
        return pointers

    def instantiate(self, g_object):
        """ 0      no fields
            1      fixed fields only (all containing pointers)
            2      indexable fields only (all containing pointers)
            3      both fixed and indexable fields (all containing pointers)
            4      both fixed and indexable weak fields (all containing pointers).

            5      unused
            6      indexable word fields only (no pointers)
            7      indexable long (64-bit) fields (only in 64-bit images)

         8-11      indexable byte fields only (no pointers) (low 2 bits are low 2 bits of size)
        12-15     compiled methods:
                       # of literal oops specified in method header,
                       followed by indexable bytes (same interpretation of low 2 bits as above)
        """
        # the instantiate call circumvents the constructors
        # and makes empty objects
        if self.ischar(g_object):
            return objectmodel.instantiate(W_Character)
        elif self.isblockclosure(g_object):
            return objectmodel.instantiate(W_BlockClosure)
        elif self.ispointers(g_object):
            return objectmodel.instantiate(W_PointersObject)
        elif g_object.format == 5:
            raise error.CorruptImageError("Unknown format 5")
        elif self.isfloat(g_object):
            return objectmodel.instantiate(W_Float)
        elif self.issignedinteger(g_object):
            return objectmodel.instantiate(W_SmallInteger)
        elif self.isunsignedinteger(g_object):
            return objectmodel.instantiate(W_LargeIntegerWord)
        elif self.isbiginteger(g_object):
            return objectmodel.instantiate(W_LargeIntegerBig)
        elif self.iswords(g_object):
            return objectmodel.instantiate(W_WordsObject)
        elif g_object.format == 7:
            raise error.CorruptImageError("Unknown format 7, no 64-bit support yet :-)")
        elif self.isbytes(g_object):
            return objectmodel.instantiate(W_BytesObject)
        elif self.iscompiledmethod(g_object):
            return objectmodel.instantiate(W_PreSpurCompiledMethod)
        else:
            assert 0, "not reachable"

    def isbytes(self, g_object):
        return 8 <= g_object.format <= 11

    def ischar(self, g_object):
        g_char = self.special_g_object_safe(constants.SO_CHARACTER_CLASS)
        return (self.ispointers(g_object) and g_object.g_class == g_char)

    def iswords(self, g_object):
        return g_object.format == 6

    def isblockclosure(self, g_object):
        g_closure = self.special_g_object_safe(constants.SO_BLOCKCLOSURE_CLASS)
        return self.ispointers(g_object) and g_object.g_class == g_closure

    def ispointers(self, g_object):
        return g_object.format < 5

    def isweak(self, g_object):
        return g_object.format == 4

    def iscompiledmethod(self, g_object):
        return 12 <= g_object.format <= 15

    def literal_count_of_method_header(self, untagged_header):
        _, literalsize, _, _, _ = constants.decode_compiled_method_header(untagged_header)
        return literalsize

class AncientReader(NonSpurReader):
    """Reader strategy for pre-4.0 images"""
    pass

class SpurReader(BaseReaderStrategy):

    FREE_OBJECT_CLASS_INDEX_PUN = 0

    def __init__(self, imageReader, version, stream, space):
        BaseReaderStrategy.__init__(self, imageReader, version, stream, space)
        space.is_spur.activate()

    def continue_read_header(self):
        BaseReaderStrategy.continue_read_header(self)
        self.hdrNumStackPages = self.stream.next_short()
        self.hdrCogCodeSize = self.stream.next_short()
        self.hdrEdenBytes = self.stream.next()  # nextWord32
        self.hdrMaxExtSemTabSize = self.stream.next_short()
        self.stream.skipbytes(2)  # unused, realign to word boundary
        self.firstSegSize = self.stream.next()
        self.freeOldSpaceInImage = self.stream.next()

    _SLOTS_MASK = 0xFFL << 56
    SLOTS_MASK = intmask(_SLOTS_MASK) if system.IS_64BIT else r_ulonglong(_SLOTS_MASK)

    def read_body(self):
        self.stream.reset_count()
        segmentEnd = self.firstSegSize
        currentAddressSwizzle = self.oldbaseaddress
        self._progress.next_stage(self.stream.length())
        while self.stream.count < segmentEnd:
            while self.stream.count < segmentEnd - 16:
                chunk, pos = self.read_object()
                self._progress.update(self.stream.count)
                if chunk.classid == self.FREE_OBJECT_CLASS_INDEX_PUN:
                    continue # ignore free chunks
                self.chunklist.append(chunk)
                self.chunks[pos + currentAddressSwizzle] = chunk
            self.log("bridge: %s (%s)" % (self.stream.count, self.stream.count + currentAddressSwizzle))
            # read bridge
            bridge = r_uint64(self.stream.next_qword())
            if bridge & self.SLOTS_MASK == 0:
                bridgeSpan = 0
            else:
                bridgeSpan = intmask(r_uint64(bridge & ~self.SLOTS_MASK))
            nextSegmentSize = intmask(r_uint64(self.stream.next_qword()))
            self.log("bridgeSpan: %s; nextSegmentSize: %s" % (bridgeSpan, nextSegmentSize))
            assert bridgeSpan >= 0
            assert nextSegmentSize >= 0
            assert self.stream.count == segmentEnd
            # if nextSegmentSize is zero, the end of the image has been reached
            if nextSegmentSize == 0:
                self.log("last segment end: %s " % (segmentEnd + currentAddressSwizzle))
                break
            segmentEnd = segmentEnd + nextSegmentSize
            # address swizzle is in bytes, but bridgeSpan is in image words
            currentAddressSwizzle += (bridgeSpan * (8 if self.version.is_64bit else 4))
        self.stream.close()
        return self.chunklist # return for testing

    def read_object(self):
        # respect new header format
        pos = self.stream.count
        assert pos % 8 == 0, "every object must be 64-bit aligned"
        headerWord = self.stream.next_qword()
        classid_l, _, format_l, _, hash_l, _, size_l = splitter[22,2,5,3,22,2,8](headerWord)
        classid, format, hash = intmask(classid_l), intmask(format_l), intmask(hash_l)
        OVERFLOW_SLOTS = 255
        if size_l == OVERFLOW_SLOTS:
            size_l = headerWord & ~self.SLOTS_MASK
            pos = self.stream.count
            classid_l, _, format_l, _, hash_l, _, overflow_size = splitter[22,2,5,3,22,2,8](self.stream.next_qword())
            classid, format, hash = intmask(classid_l), intmask(format_l), intmask(hash_l)
            assert overflow_size == OVERFLOW_SLOTS, "objects with long header must have 255 in slot count"
        size = r_uint(r_uint32(size_l)) # reading 64 bit images not supported in 32 bit build
        assert 0 <= format <= 31
        chunk = ImageChunk(size, format, classid, hash)
        # the minimum object length is 16 bytes, i.e. 8 header + 8 payload
        # (to accommodate a forwarding ptr)
        chunk.data = [self.stream.next() for _ in range(self.words_for(size))]
        if len(chunk.data) != size:
            # remove trailing alignment slots
            assert size < len(chunk.data) and len(chunk.data) - size < 4
            chunk.data = chunk.data[:size]
        if format < 10 and classid != self.FREE_OBJECT_CLASS_INDEX_PUN:
            for slot in chunk.data:
                assert slot % 16 != 0 or slot >= self.oldbaseaddress
        assert format != 0 or classid == 0 or size == 0, "empty objects must not have slots"
        return chunk, pos

    def words_for(self, size):
        # see Spur32BitMemoryManager>>smallObjectBytesForSlots:
        if size <= 1:
            return 2
        else:
            return size + (size & 1)

    def g_class_of(self, chunk):
        major_class_index = self.major_class_index_of(chunk.classid)
        minor_class_index = self.minor_class_index_of(chunk.classid)
        HIDDEN_ROOTS_CHUNK = 4 # after nil, true, false, freeList
        hiddenRoots = self.chunklist[HIDDEN_ROOTS_CHUNK]
        classTablePage = self.chunks[hiddenRoots.data[major_class_index]]
        return self.chunks[classTablePage.data[minor_class_index]].g_object

    def major_class_index_of(self, classid):
        return classid >> 10

    def minor_class_index_of(self, classid):
        return classid & ((1 << 10) - 1)

    def decode_pointers(self, g_object, space, end=-1):
        if end == -1:
            end = len(g_object.chunk.data)
        pointers = []
        for i in range(end):
            pointer = g_object.chunk.data[i]
            if (pointer & 3) == 0:
                # pointer = ...00
                try:
                    pointers.append(self.chunks[pointer].g_object)
                except KeyError:
                    print "WARN: Bogus pointer: %d. Treating as small int." % pointer
                    small_int = GenericObject()
                    small_int.initialize_int(pointer >> 1, self, space)
                    pointers.append(small_int)
            elif (pointer & 1) == 1:
                # pointer = ....1
                # tagged integer
                small_int = GenericObject()
                small_int.initialize_int(pointer >> 1, self, space)
                pointers.append(small_int)
            else:
                #assert (pointer & 3) == 2
                # pointer = ...10
                # immediate character
                character = GenericObject()
                character.initialize_char(pointer >> 2, self, space)
                pointers.append(character)
        return pointers

    def instantiate(self, g_object):
        """ 0      no fields
            1      fixed fields only (all containing pointers)
            2      indexable fields only (all containing pointers)
            3      both fixed and indexable fields (all containing pointers)
            4      indexable weak fields (all containing pointers)
            5      fixed weak fields (all containing pointers)
            6-8    unused

            9      indexable 64 bit fields (no pointers)
            10-11  indexable 32 bit fields (no pointers)
            12-15  indexable 16 bit fields (no pointers)
            16-23  indexable byte fields (no pointers)
                     for the above, the lower bits are the lower bits of the size
            24-31  compiled methods:
                     # of literal oops specified in method header,
                     followed by indexable bytes (same interpretation of low bits as above)
        """
        # the instantiate call circumvents the constructors
        # and makes empty objects
        # timfel: sorted by likelyhood so the JIT can generate better checks
        if self.ischar(g_object):
            return objectmodel.instantiate(W_Character)
        elif self.isblockclosure(g_object):
            return objectmodel.instantiate(W_BlockClosure)
        elif self.ispointers(g_object):
            return objectmodel.instantiate(W_PointersObject)
        elif self.isfloat(g_object):
            return objectmodel.instantiate(W_Float)
        elif self.issignedinteger(g_object):
            return objectmodel.instantiate(W_SmallInteger)
        elif self.isunsignedinteger(g_object):
            return objectmodel.instantiate(W_LargeIntegerWord)
        elif self.isbiginteger(g_object):
            return objectmodel.instantiate(W_LargeIntegerBig)
        elif self.iswords(g_object):
            return objectmodel.instantiate(W_WordsObject)
        elif self.isbytes(g_object):
            return objectmodel.instantiate(W_BytesObject)
        elif self.iscompiledmethod(g_object):
            return objectmodel.instantiate(W_SpurCompiledMethod)
        elif g_object.format in (6, 7, 8):
            raise error.CorruptImageError("Unknown format " + str(g_object.format))
        else:
            assert 0, "not reachable"

    def ischar(self, g_object):
        g_char = self.special_g_object_safe(constants.SO_CHARACTER_CLASS)
        return (self.ispointers(g_object) and g_object.g_class == g_char)

    def isblockclosure(self, g_object):
        g_closure = self.special_g_object_safe(constants.SO_BLOCKCLOSURE_CLASS)
        return self.ispointers(g_object) and g_closure == g_object.g_class

    def ispointers(self, g_object):
        return g_object.format < 6

    def isweak(self, g_object):
        return 4 <= g_object.format <= 5

    def iswords(self, g_object):
        if not system.IS_64BIT and g_object.format == 9:
            # 64-bit words objects are not supported in our 32-bit VM, because
            # we mush them all together
            self.log("Warning: a 64bit-words object is being truncated to 32-bits.")
        return 9 <= g_object.format <= 15

    def isbytes(self, g_object):
        return 16 <= g_object.format <= 23

    def iscompiledmethod(self, g_object):
        return 24 <= g_object.format <= 31

    def literal_count_of_method_header(self, untagged_header):
        return untagged_header & 0x7fff # AlternateHeaderNumLiteralsMask
# ____________________________________________________________

class SqueakImage(object):
    _immutable_fields_ = [
        "space",
        "w_asSymbol",
        "version",
        "startup_time",
        "w_simulatePrimitive",
    ]

    def __init__(self, reader):
        space = self.space = reader.space
        self.w_asSymbol = self.find_symbol(space, reader, "asSymbol")
        self.lastWindowSize = reader.lastWindowSize
        self.version = reader.version
        self.startup_time = time.time()
        from rsqueakvm.plugins.simulation import SIMULATE_PRIMITIVE_SELECTOR
        self.w_simulatePrimitive = self.find_symbol(space, reader, SIMULATE_PRIMITIVE_SELECTOR)

    def find_symbol(self, space, reader, symbol):
        w_dnu = space.w_doesNotUnderstand
        assert isinstance(w_dnu, W_BytesObject)
        assert space.unwrap_string(w_dnu) == "doesNotUnderstand:"
        w_Symbol = w_dnu.getclass(space)
        w_obj = None
        # bit annoying that we have to hunt through the image :-(
        for chunk in reader.chunklist:
            w_obj = chunk.g_object.w_object
            if not isinstance(w_obj, W_BytesObject):
                continue
            if not w_obj.getclass(space).is_same_object(w_Symbol):
                continue
            if space.unwrap_string(w_obj) == symbol:
                return w_obj
        w_obj = space.w_nil
        return w_obj

    def special(self, index):
        if index >= self.space.w_special_objects.size():
            return None
        else:
            return self.space.w_special_objects.at0(self.space, index)

# ____________________________________________________________

class GenericObject(object):
    """ Intermediate representation of squeak objects. To establish all
        pointers as object references, ImageReader creates instances of
        GenericObject from the image chunks, and uses them as starting
        point for the actual create of rsqueakvm.model classes.
        """

    def __init__(self):
        self.reader = None
        self.filled_in = False
        self.filled_in_weak = False

    def isinitialized(self):
        return self.reader is not None

    def initialize_int(self, value, reader, space):
        self.reader = reader
        self.size = 0
        if value in reader.intcache:
            w_int = reader.intcache[value]
        else:
            w_int = space.wrap_int(value)
            reader.intcache[value] = w_int
        self.w_object = w_int
        self.filled_in = True

    def initialize_char(self, untagged_value, reader, space):
        self.reader = reader
        self.size = 0
        self.w_object = W_Character(untagged_value)
        self.filled_in = True

    def initialize(self, chunk, reader, space):
        self.reader = reader
        self.size = chunk.size
        self.hash = chunk.hash
        self._format = chunk.format
        self.chunk = chunk # for bytes, words and compiledmethod
        self.init_class()
        self.init_data(space)  # for pointers
        self.w_object = None

    @property
    def format(self):
        return jit.promote(self._format)

    def __repr__(self):
        return "<GenericObject %s>" % ("uninitialized" if not self.isinitialized()
                else self.w_object if hasattr(self, "w_object") and self.w_object
                else "size=%d hash=%d format=%d" % (self.size, self.hash, self.format))

    def init_class(self):
        self.g_class = self.reader.g_class_of(self.chunk)

    def init_data(self, space):
        if self.reader.ispointers(self):
            self.pointers = self.reader.decode_pointers(self, space)
            assert None not in self.pointers
        elif self.reader.iscompiledmethod(self):
            header = self.chunk.data[0] >> 1 # untag tagged int
            literalsize = self.reader.literal_count_of_method_header(header)
            self.pointers = self.reader.decode_pointers(self, space, literalsize + 1)  # adjust +1 for the header

    def init_w_object(self, space):
        if self.w_object is None:
            self.w_object = self.reader.instantiate(self)
        return self.w_object

    def isweak(self):
        return self.reader.isweak(self)

    def len_bytes(self):
        sz = self.reader.len_bytes_of(self.chunk)
        return sz - (self.format & 3)

    def get_bytes(self):
        bytes = self.reader.get_bytes_of(self.chunk)
        stop = len(bytes) - (self.format & 3)
        assert stop >= 0
        return bytes[:stop] # omit odd bytes

    def get_ruints(self, required_len=-1):
        from rpython.rlib.rarithmetic import r_uint32, r_uint
        words = [r_uint(r_uint32(x)) for x in self.chunk.data]
        if required_len != -1 and len(words) != required_len:
            raise error.CorruptImageError("Expected %d words, got %d" % (required_len, len(words)))
        return words

    def fillin(self, space):
        if not self.filled_in:
            self.filled_in = True
            self.w_object.fillin(space, self)

    def fillin_weak(self, space):
        if not self.filled_in_weak and self.isweak():
            self.filled_in_weak = True
            self.w_object.fillin_weak(space, self)

    def fillin_finalize(self, space):
        self.w_object.fillin_finalize(space, self)

    def get_g_pointers(self):
        assert self.pointers is not None
        return self.pointers

    def get_pointers(self):
        return [g_object.w_object for g_object in self.get_g_pointers()]

    def get_class(self):
        w_class = self.g_class.w_object
        assert isinstance(w_class, W_PointersObject)
        return w_class

    def get_hash(self):
        return self.chunk.hash

    def as_string(self):
        """NOT RPYTHON"""
        return "".join([chr(c) for bytes in
            [splitter[8,8,8,8](w) for w in self.chunk.data]
            for c in bytes if c != 0])

    def classname(self):
        """NOT RPYTHON"""
        return self.g_class.pointers[6].as_string()


class ImageChunk(object):
    """ A chunk knows the information from the header, but the body of the
    object is not decoded yet."""
    def __init__(self, size, format, classid, hash, data=None):
        self.size = size
        self.format = format
        self.classid = classid
        self.hash = hash
        # list of integers forming the body of the object
        self.data = data
        self.g_object = GenericObject()

    def __repr__(self):
        return "ImageChunk(size=%(size)d, format=%(format)d, " \
                "classid=%(classid)d, hash=%(hash)d, data=%(data)r)" \
                % self.__dict__

    def __eq__(self, other):
        "(for testing)"
        return (self.__class__ is other.__class__ and
                self.format == other.format and
                self.classid == other.classid and
                self.hash == other.hash and
                self.data == other.data)

    def __ne__(self, other):
        "(for testing)"
        return not self == other

    def as_g_object(self, reader, space):
        if not self.g_object.isinitialized():
            self.g_object.initialize(self, reader, space)
        return self.g_object

    def iscompact(self):
        # pre-Spur
        return 0 < self.classid < 32

class SpurImageWriter(object):
    _immutable_fields_ = ["space", "image", "trace_queue", "oop_map"]
    # XXX: Writes forcibly little-endian 32-bit Spur-format images
    image_header_size = 64
    word_size = 4

    def __init__(self, interp, filename):
        from rpython.rlib import streamio, objectmodel
        self.space = interp.space
        self.image = interp.image
        self.f = streamio.open_file_as_stream(filename, mode="wb")
        self.next_chunk = self.image_header_size
        self.oop_map = {}
        self.trace_queue = []
        self.hidden_roots = None

    @objectmodel.specialize.argtype(1)
    def len_and_header(self, obj):
        import math
        n = self.fixed_and_indexable_size_for(obj)
        if isinstance(obj, W_BytesObject) or isinstance(obj, W_LargeInteger) or isinstance(obj, W_CompiledMethod):
            size = int(math.ceil(n / float(self.word_size)))
        else:
            size = n
        if size < 255:
            return n, size + 2, 2
        else:
            return n, size + 2, 4

    def frame_size_for(self, obj):
        w_method = None
        if obj.getclass(self.space).is_same_object(self.space.w_MethodContext):
            w_method = obj.fetch(self.space, constants.MTHDCTX_METHOD)
            if not w_method.is_nil(self.space):
                w_method.compute_frame_size()
        elif obj.getclass(self.space).is_same_object(self.space.w_BlockContext):
            w_home = obj.fetch(self.space, constants.BLKCTX_HOME_INDEX)
            return self.frame_size_for(w_home)
        return constants.COMPILED_METHOD_FULL_FRAME_SIZE

    @objectmodel.specialize.argtype(1)
    def fixed_and_indexable_size_for(self, obj):
        if (isinstance(obj, W_PointersObject) and
            (obj.getclass(self.space).is_same_object(self.space.w_MethodContext) or
             obj.getclass(self.space).is_same_object(self.space.w_BlockContext))):
            return obj.instsize() + self.frame_size_for(obj)
        elif isinstance(obj, W_SpurCompiledMethod):
            return obj.varsize()
        elif isinstance(obj, W_PreSpurCompiledMethod):
            if obj.primitive() != 0:
                return obj.varsize() + 3  # account for three extra bytes with
                                          # primitive idx
            else:
                return obj.varsize()
        else:
            return obj.instsize() + obj.varsize()

    def padding_for(self, length):
        if length - 2 == 0:
            return 8
        elif (length % 2 != 0 and self.word_size == 4):
            return 4
        else:
            return 0

    def trace_image(self, s_frame):
        w_active_process = wrapper.scheduler(self.space).active_process()
        active_process = wrapper.ProcessWrapper(self.space, w_active_process)
        active_process.store_suspended_context(s_frame.w_self())
        try:
            # The first objects need to be in this order:
            # 1. nil
            # 2. false
            # 3. true
            # 4. free list
            # 5. hidden roots
            # 6. special objects array
            self.reserve(self.space.w_nil)
            self.reserve(self.space.w_false)
            self.reserve(self.space.w_true)
            # free list object. we need a word array kind of thing. Bitmaps are like that
            self.reserve(W_WordsObject(self.space, self.space.w_Bitmap, self.word_size * 8))
            self.hidden_roots = W_PointersObject(self.space, self.space.w_Array, 2**12 + 8)
            self.reserve(self.hidden_roots)
            w_special_objects = self.space.w_special_objects
            for i in range(w_special_objects.size()):
                w_obj = w_special_objects.fetch(self.space, i)
                if isinstance(w_obj, W_SmallInteger):
                    # This cannot be...
                    val = self.space.unwrap_int(w_obj)
                    if val >= 0:
                        w_cls = self.space.w_LargePositiveInteger
                    else:
                        w_cls = self.space.w_LargeNegativeInteger
                    w_special_objects.store(
                        self.space, i,
                        W_LargeIntegerWord(self.space, w_cls, r_uint(val), 4))
            self.reserve(w_special_objects)
            self.trace_until_finish()
            # tracing through the image will have populated the hidden roots and
            # its classtables. write the hidden roots object again, which
            # triggers writing its classtables
            assert len(self.trace_queue) == 0
            self.trace_queue.append(self.hidden_roots)
            self.trace_until_finish()
            self.write_last_bridge()
            self.write_file_header(w_special_objects)
        finally:
            self.f.close()
            active_process.store_suspended_context(self.space.w_nil)

    @jit.dont_look_inside
    def trace_until_finish(self):
        while True:
            if len(self.trace_queue) == 0:
                break
            obj = self.trace_queue.pop()
            self.write_and_trace(obj)

    def write_file_header(self, w_special_objects):
        sp_obj_oop = self.oop_map[w_special_objects][0]
        image_header_size = 64 if self.word_size == 4 else 128
        displaysize = self.image.lastWindowSize
        hdrflags = (0 +  # 0/1 fullscreen or not
                    0b10 +  # 0/2 imageFloatsLittleEndian or not
                    0x10 +  # preemption does not yield
                    0)  # old finalization
        self.f.seek(0, 0)
        version = 6521
        if self.space.uses_block_contexts.is_set():
            version = 0x1234  # our custom version magic
        self.write_word(version)
        self.write_word(image_header_size)  # hdr size
        self.write_word(self.next_chunk - image_header_size)  # memory size
        self.write_word(image_header_size)  # start of memory
        self.write_word(sp_obj_oop)
        self.write_word(0xffee)  # last hash
        self.write_word(displaysize)
        self.write_word(hdrflags)
        self.write_word(0)  # extra VM memory
        self.write_word(0)  # (num stack pages << 16) | cog code size
        self.write_word(0)  # eden bytes
        self.write_word(0)  # max ext semaphore size << 16
        self.write_word(self.next_chunk - image_header_size)  # first segment size
        self.write_word(0)  # free old space in image
        self.write_word(0)  # padding
        self.write_word(0)  # padding

    def write_last_bridge(self):
        self.f.seek(self.next_chunk, 0)
        self.next_chunk = self.next_chunk + 16
        # put the magic FINAL BRIDGE header
        # FIXME: 64bit??
        self.write_word((1 << 30) + (10 << 24) + 3)
        self.write_word(0)
        self.write_word(0)
        self.write_word(0)

    def insert_class_into_classtable(self, obj):
        classhash = obj.gethash()
        majoridx = classhash >> 10
        minoridx = classhash & ((1 << 10) - 1)
        page = self.hidden_roots.fetch(self.space, majoridx)
        if page.is_nil(self.space):
            page = W_PointersObject(self.space, self.space.w_Array, 2**10)
            self.hidden_roots.store(self.space, majoridx, page)
        # XXX: TODO: Why does this happen??
        # assert page.fetch(self.space, minoridx).is_nil(self.space)
        page.store(self.space, minoridx, obj)

    @objectmodel.specialize.argtype(1)
    def write_and_trace(self, obj):
        if obj.is_class(self.space):
            self.insert_class_into_classtable(obj)
        # always make sure we're tracing our own class, too this is really
        # important for metaclasses and old images, where a compact class might
        # not otherwise be traced, because it would be in the header.
        self.reserve(obj.getclass(self.space))

        oop, length, hdrsize, sz, padding = self.oop_map[obj]
        self.write_header(hdrsize, sz, obj, oop)

        assert self.f.tell() == (oop + (2 * self.word_size))

        if isinstance(obj, W_BytesObject) or isinstance(obj, W_LargeInteger):
            self.write_bytes_object(obj)
        elif isinstance(obj, W_WordsObject) or isinstance(obj, W_DisplayBitmap) or isinstance(obj, W_Float):
            self.write_words_object(obj)
        elif isinstance(obj, W_CompiledMethod):
            self.write_compiled_method(obj)
        else:
            self.write_pointers_object(obj)

        self.f.write("\0" * padding)

        assert self.f.tell() == oop + length * self.word_size + padding

    @objectmodel.specialize.argtype(1)
    def reserve(self, obj):
        if isinstance(obj, W_SmallInteger):
            newoop = 0
            if obj.value >= 0:
                if obj.value <= constants.TAGGED_MAXINT32:
                    newoop = (obj.value << 1) + 1
                else:
                    return self.reserve(W_LargeIntegerWord(
                        self.space, self.space.w_LargePositiveInteger,
                        r_uint(obj.value), constants.BYTES_PER_MACHINE_INT))
            else:
                if obj.value >= constants.TAGGED_MININT32:
                    newoop = intmask((((r_int64(1) << 31) + obj.value) << 1) + 1)
                else:
                    return self.reserve(W_LargeIntegerWord(
                        self.space, self.space.w_LargeNegativeInteger,
                        r_uint(obj.value), constants.BYTES_PER_MACHINE_INT))
            return (newoop, 0, 0, 0, 0)
        elif isinstance(obj, W_Character):
            assert obj.value < constants.TAGGED_MAXINT32
            return ((obj.value << 2) + 0b10, 0, 0, 0, 0)
        else:
            oop = self.oop_map.get(obj, (0, 0, 0, 0, 0))
            if oop[0] > 0:
                return oop
            else:
                sz, length, hdrsize = self.len_and_header(obj)
                oop = self.next_chunk + (hdrsize - 2) * self.word_size
                padding = self.padding_for(length)
                self.next_chunk = oop + length * self.word_size + padding
                retval = (oop, length, hdrsize, sz, padding)
                self.oop_map[obj] = retval
                self.trace_queue.append(obj)
                if (not self.space.is_spur.is_set()) and obj.is_class(self.space):
                    # rehash all classes in non-spur images, so we don't get
                    # collisions
                    obj.rehash()
                return retval

    def write_bytes_object(self, obj):
        self.f.write(self.space.unwrap_string(obj))
        paddingbytes = self.word_size - (obj.size() % self.word_size)
        if paddingbytes != self.word_size:
            self.f.write("\0" * paddingbytes)

    def write_words_object(self, obj):
        self.f.write(self.space.unwrap_string(obj))
        if self.word_size == 8 and (obj.size() % 2 == 1):
            self.f.write("\0" * 4)

    def write_compiled_method(self, obj):
        cmbytes = obj.getbytes()
        if self.space.is_spur.is_set():
            self.write_word((obj.getheader() << 1) + 1) # header is saved as tagged int
        else:
            newheader = (obj.literalsize # 15 bits
                         | (0 << 15)  # is optimized, 1 bit
                         | ((1 if (obj.primitive() != 0) else 0) << 16)  # 1 bit
                         | ((1 if obj.islarge else 0) << 17)  # 1 bit
                         | (obj.tempsize() << 18)  # 6 bits
                         | (obj.argsize << 24)  # 4 bits
                         | (0 << 28)  # access mod, 2 bits
                         | (0 << 30))  # instruction set bit, 1 bit
            self.write_word((newheader << 1) + 1)  # header is saved as tagged int
        for i in range(obj.getliteralsize() / constants.BYTES_PER_WORD):
            self.write_word(self.reserve(obj.getliteral(i))[0])
        paddingbytes = 0
        if (not self.space.is_spur.is_set()) and obj.primitive() != 0:
            # we must insert the primitive bytecode and index into the first
            # three bytes
            self.f.write(chr(139))  # call prim bytecode
            self.f.write(chr(obj.primitive() & 255))  # lower bits
            self.f.write(chr((obj.primitive() >> 8) & 255))  # higher bits
            paddingbytes = self.word_size - ((len(cmbytes) + 3) % self.word_size)
        else:
            paddingbytes = self.word_size - (len(cmbytes) % self.word_size)
        self.f.write("".join(cmbytes))
        if paddingbytes != self.word_size:
            self.f.write("\0" * paddingbytes)

    def write_pointers_object(self, obj):
        if (not self.space.is_spur.is_set()) and obj.is_class(self.space) and (obj.size() > constants.CLASS_FORMAT_INDEX):
            # we must retrofit the new class format
            # The classformat in Spur, as an integer value, is:
            # <5 bits inst spec><16 bits inst size>
            w_oldfmt = obj.fetch(self.space, constants.CLASS_FORMAT_INDEX)
            oldfmt = self.space.unwrap_int(w_oldfmt)
            instsize_lo = (oldfmt >> 1) & 0x3F
            instsize_hi = (oldfmt >> (9 + 1)) & 0xC0
            oldinstsize = (instsize_lo | instsize_hi) - 1  # subtract hdr
            instspec = self.convert_instspec_to_spur((oldfmt >> 7) & 15)
            newfmt = ((instspec & 0x1f) << 16) | (oldinstsize & 0xffff)
            w_newfmt = self.space.wrap_int(newfmt)
            for i in range(obj.size()):
                if i != constants.CLASS_FORMAT_INDEX:
                    self.write_word(self.reserve(obj.fetch(self.space, i))[0])
                else:
                    self.write_word(self.reserve(w_newfmt)[0])
        else:
            for i in range(obj.size()):
                self.write_word(self.reserve(obj.fetch(self.space, i))[0])
        if (obj.getclass(self.space).is_same_object(self.space.w_MethodContext) or
            obj.getclass(self.space).is_same_object(self.space.w_BlockContext)):
            # fill out nils beyond the knowable end of stack
            for i in range(self.frame_size_for(obj) - obj.varsize()):
                self.write_word(self.reserve(self.space.w_nil)[0])

    def write_word(self, word):
        # FIXME: 64bit??
        self.f.write("".join(
            [chr(word & r_uint(0x000000ff)),
             chr((word & r_uint(0x0000ff00)) >> 8),
             chr((word & r_uint(0x00ff0000)) >> 16),
             chr((word & r_uint(0xff000000)) >> 24)]))

    def write_header(self, hdrsize, sz, obj, oop):
        self.f.seek(oop - ((hdrsize - 2) * self.word_size), 0)
        self.f.write(self.headers_for_hash_numfields(
            obj.getclass(self.space),
            obj.gethash(),
            sz))

    # conversion map from old formats to new formats
    old_to_spur_specs = [0,1,2,3,4,-1,10,9,16,16,16,16,24,24,24,24]
    def convert_instspec_to_spur(self, spec):
        fmt = self.old_to_spur_specs[spec]
        assert fmt >= 0
        # if fmt == 4 and not Class.isvariable():
        #     fmt = 5 # weak objects now split in fixed and indexable types
        return fmt

    def headers_for_hash_numfields(self, Class, Hash, size):
        import math
        from rsqueakvm.storage_classes import BYTES, COMPILED_METHOD, LARGE_INTEGER
        classshadow = Class.as_class_get_shadow(self.space)
        length = r_uint64(size)
        wordlen = size
        fmt = 0
        w_fmt = Class.fetch(self.space, constants.CLASS_FORMAT_INDEX)
        assert isinstance(w_fmt, W_SmallInteger)
        if self.space.is_spur.is_set():
            fmt = (w_fmt.value >> 16) & 0x1f
        else:
            fmt = self.convert_instspec_to_spur((w_fmt.value >> 7) & 15)
        if (classshadow.instance_kind == BYTES or
            classshadow.instance_kind == COMPILED_METHOD or
            classshadow.instance_kind == LARGE_INTEGER):
            wordlen = int(math.ceil(size / 4.0))
            length = r_uint64(wordlen)
            fmt = fmt | ((wordlen * 4) - size)
        header = r_uint64(0)
        length_header = r_uint64(0)
        if wordlen >= 255:
            length_header = r_uint64(length | (r_uint64(0xff) << 56))
            length = r_uint64(0xff)
        header = header | ((length << 56) |
                           (r_uint64(Hash) << 32) |
                           (r_uint64(fmt) << 24) |
                           (r_uint64(Class.gethash())))

        if wordlen >= 255:
            extra_bytes = self.ruint64_tobytes(length_header)
            header_bytes = self.ruint64_tobytes(header)
            return extra_bytes + header_bytes
        else:
            return self.ruint64_tobytes(header)

    def ruint64_tobytes(self, i):
        res = ['\0'] * 8
        value = i
        mask = r_uint64(0xff)
        for i in range(8):
            res[i] = chr(intmask(value & mask))
            value >>= 8
        return "".join(res)
