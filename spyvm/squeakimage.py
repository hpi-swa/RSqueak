import os, time
from spyvm import constants, model, util, error
from spyvm.util import stream, system
from spyvm.util.bitmanipulation import splitter
from rpython.rlib import objectmodel
from rpython.rlib.rarithmetic import r_ulonglong, intmask, r_uint

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

    def __init__(self, magic, is_big_endian, is_64bit, has_closures, has_floats_reversed, is_spur=False):
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

class ImageReader(object):

    def __init__(self, space, stream):
        self.space = space
        self.stream = stream
        self.version = None
        self.readerStrategy = None

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
    def special_w_objects(self):
        return self.readerStrategy.special_w_objects

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

    def chunk(self, pointer):
        return self.readerStrategy.chunk(pointer)

    def decode_pointers(self, g_object, space, end=-1):
        return self.readerStrategy.decode_pointers(g_object, space, end)

class BaseReaderStrategy(object):

    def __init__(self, imageReader, version, stream, space):
        self.imageReader = imageReader
        self.version = version
        self.stream = stream
        self.space = space
        self.chunks = {} # Dictionary mapping old address to chunk object
        self.chunklist = [] # Flat list of all read chunks
        self.intcache = {} # Cached instances of SmallInteger
        self.lastWindowSize = 0
        self.filledin_objects = 0
        self.filledin_weakobjects = 0

    def log_progress(self, progress, char):
        if progress % 1000 == 0:
            os.write(2, char)

    def continue_read_header(self):
        # 1 word headersize
        self.headersize = self.stream.next()
        # 1 word size of the full image
        self.endofmemory = self.stream.next() # endofmemory = bodysize
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
        self.populate_special_objects()
        self.fillin_weak_w_objects()

    def read_body(self):
        raise NotImplementedError("subclass must override this")

    def init_compactclassesarray(self):
        raise NotImplementedError("subclass must override this")

    def g_class_of(self, chunk):
        raise NotImplementedError("subclass must override this")

    def init_g_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.as_g_object(self, self.space) # initialize g_object
        self.special_g_objects = self.chunks[self.specialobjectspointer].g_object.pointers

    def assign_prebuilt_constants(self):
        # Assign classes and objects that in special objects array that are already created.
        self._assign_prebuilt_constants(constants.objects_in_special_object_table, self.space.objtable)
        self._assign_prebuilt_constants(constants.classes_in_special_object_table, self.classtable())

    def classtable(self):
        # this is overridden by AncientReader
        return self.space.classtable

    def _assign_prebuilt_constants(self, names_and_indices, prebuilt_objects):
        for name, so_index in names_and_indices.items():
            name = "w_" + name
            if name in prebuilt_objects:
                try:
                    w_object = prebuilt_objects[name]
                    g_object = self.special_g_object(so_index)
                    if g_object.w_object is None:
                        g_object.w_object = w_object
                    else:
                        if not g_object.w_object.is_nil(self.space):
                           raise Warning('Object found in multiple places in the special objects array')
                except IndexError:
                    # certain special objects might not yet be in the image's table
                    pass

    def special_g_object(self, index):
        # while python would raise an IndexError, after translation a nonexisting key results in a segfault...
        if index >= len(self.special_g_objects):
            raise IndexError
        return self.special_g_objects[index]

    def init_w_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.init_w_object(self.space)
        self.special_w_objects = [g.w_object for g in self.special_g_objects]

    def populate_special_objects(self):
        self.space.populate_special_objects(self.special_w_objects)

    def fillin_w_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.fillin(self.space)

    def fillin_weak_w_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.fillin_weak(self.space)

    def log_object_filledin(self):
        self.filledin_objects = self.filledin_objects + 1
        self.log_progress(self.filledin_objects, '%')

    def log_weakobject_filledin(self):
        self.filledin_weakobjects = self.filledin_weakobjects + 1
        self.log_progress(self.filledin_weakobjects * 100, '*')

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

class NonSpurReader(BaseReaderStrategy):

    def read_body(self):
        self.stream.reset_count()
        while self.stream.count < self.endofmemory:
            chunk, pos = self.read_object()
            self.log_progress(len(self.chunklist), '#')
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

    def chunk(self, pointer):
        return self.chunks[pointer]

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
                pointers.append(self.chunk(pointer).g_object)
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
            return objectmodel.instantiate(model.W_Character)
        elif self.ispointers(g_object):
            return objectmodel.instantiate(model.W_PointersObject)
        elif g_object.format == 5:
            raise error.CorruptImageError("Unknown format 5")
        elif self.isfloat(g_object):
            return objectmodel.instantiate(model.W_Float)
        elif self.is32bitlargepositiveinteger(g_object):
            return objectmodel.instantiate(model.W_LargePositiveInteger1Word)
        elif self.iswords(g_object):
            return objectmodel.instantiate(model.W_WordsObject)
        elif g_object.format == 7:
            raise error.CorruptImageError("Unknown format 7, no 64-bit support yet :-)")
        elif self.isbytes(g_object):
            return objectmodel.instantiate(model.W_BytesObject)
        elif self.iscompiledmethod(g_object):
            return objectmodel.instantiate(model.W_PreSpurCompiledMethod)
        else:
            assert 0, "not reachable"

    def isbytes(self, g_object):
        return 8 <= g_object.format <= 11

    def is32bitlargepositiveinteger(self, g_object):
        return (g_object.format == 8 and
                self.space.w_LargePositiveInteger.is_same_object(g_object.g_class.w_object) and
                len(g_object.get_bytes()) <= 4)

    def ischar(self, g_object):
        return (self.ispointers(g_object) and
                self.space.w_Character.is_same_object(g_object.g_class.w_object))

    def iswords(self, g_object):
        return g_object.format == 6

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

    def classtable(self):
        classtable = NonSpurReader.classtable(self)
        classtable = classtable.copy()
        # In non-modern images (pre 4.0), there was no BlockClosure class.
        del classtable["w_BlockClosure"]
        return classtable

class SpurReader(BaseReaderStrategy):

    FREE_OBJECT_CLASS_INDEX_PUN = 0

    def __init__(self, imageReader, version, stream, space):
        BaseReaderStrategy.__init__(self, imageReader, version, stream, space)
        space.is_spur.activate()

    def continue_read_header(self):
        BaseReaderStrategy.continue_read_header(self)
        self.hdrNumStackPages = self.stream.next_short()
        self.hdrCogCodeSize = self.stream.next_short()
        self.hdrEdenBytes = self.stream.next() # nextWord32
        self.hdrMaxExtSemTabSize = self.stream.next_short()
        self.stream.skipbytes(2) # unused, realign to word boundary
        self.firstSegSize = self.stream.next()
        self.freeOldSpaceInImage = self.stream.next()

    _SLOTS_MASK = 0xFFL << 56
    SLOTS_MASK = intmask(_SLOTS_MASK) if system.IS_64BIT else r_ulonglong(_SLOTS_MASK)

    def read_body(self):
        self.stream.reset_count()
        segmentEnd = self.firstSegSize
        currentAddressSwizzle = self.oldbaseaddress
        while self.stream.count < segmentEnd:
            while self.stream.count < segmentEnd - 16:
                # print "head",
                chunk, pos = self.read_object()
                self.log_progress(len(self.chunklist), '#')
                if chunk.classid == self.FREE_OBJECT_CLASS_INDEX_PUN:
                    continue # ignore free chunks
                self.chunklist.append(chunk)
                self.chunks[pos + currentAddressSwizzle] = chunk
            print "bridge at", self.stream.count, "(", self.stream.count + currentAddressSwizzle, ")"
            # read bridge
            bridgeSpan = intmask(self.stream.next_qword())
            nextSegmentSize = intmask(self.stream.next_qword())
            print "bridgeSpan", bridgeSpan, "nextSegmentSize", nextSegmentSize
            # the above causes silent overflow in 32bit builds and 64bit images
            if self.version.is_64bit:
                # subtract the overflow slots bits which are 255
                bridgeSpan = intmask(bridgeSpan & ~self.SLOTS_MASK)
            assert bridgeSpan >= 0
            assert nextSegmentSize >= 0
            assert self.stream.count == segmentEnd
            # if nextSegmentSize is zero, the end of the image has been reached
            if nextSegmentSize == 0:
                print "last segment end at", segmentEnd + currentAddressSwizzle
                if self.version.is_64bit:
                    FINAL_BRIDGE_HEADER = (1 << 30) + (9 << 24) + 3
                    assert bridgeSpan == FINAL_BRIDGE_HEADER
                else:
                    FINAL_BRIDGE_HEADER = (1 << 30) + (10 << 24) + 3
                    assert bridgeSpan == FINAL_BRIDGE_HEADER
                break
            segmentEnd = segmentEnd + nextSegmentSize
            currentAddressSwizzle += bridgeSpan
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
        size = r_uint(size_l) # reading 64 bit images not supported in 32 bit build
        assert 0 <= format <= 31
        chunk = ImageChunk(size, format, classid, hash)
        # the minimum object length is 16 bytes, i.e. 8 header + 8 payload
        # (to accommodate a forwarding ptr)
        chunk.data = [self.stream.next() for _ in range(self.words_for(size))]
        if len(chunk.data) != size:
            # remove trailing alignment slots
            assert size < len(chunk.data) and len(chunk.data) - size < 4
            # BUG here, size at one point is waaay to big for 32bits
            # which curiously results in a memory error
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

    def chunk(self, pointer):
        if pointer not in self.chunks:
            # HACK: use nil by default
            print "WARNING: bogus pointer", pointer
            return self.chunklist[0]
        return self.chunks[pointer]

    def decode_pointers(self, g_object, space, end=-1):
        if end == -1:
            end = len(g_object.chunk.data)
        pointers = []
        for i in range(end):
            pointer = g_object.chunk.data[i]
            if (pointer & 3) == 0:
                # pointer = ...00
                pointers.append(self.chunk(pointer).g_object)
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
        if self.ischar(g_object):
            return objectmodel.instantiate(model.W_Character)
        elif self.ispointers(g_object):
            return objectmodel.instantiate(model.W_PointersObject)
        elif g_object.format in (6, 7, 8):
            raise error.CorruptImageError("Unknown format " + str(g_object.format))
        elif self.isfloat(g_object):
            return objectmodel.instantiate(model.W_Float)
        elif self.is32bitlargepositiveinteger(g_object):
            return objectmodel.instantiate(model.W_LargePositiveInteger1Word)
        elif self.iswords(g_object):
            return objectmodel.instantiate(model.W_WordsObject)
        elif self.isbytes(g_object):
            return objectmodel.instantiate(model.W_BytesObject)
        elif self.iscompiledmethod(g_object):
            return objectmodel.instantiate(model.W_SpurCompiledMethod)
        else:
            assert 0, "not reachable"

    def ischar(self, g_object):
        return self.space.w_Character.is_same_object(g_object.g_class.w_object)

    def ispointers(self, g_object):
        return g_object.format < 6

    def isweak(self, g_object):
        return 4 <= g_object.format <= 5

    def is32bitlargepositiveinteger(self, g_object):
        return (g_object.format == 16 and
                self.space.w_LargePositiveInteger.is_same_object(g_object.g_class.w_object) and
                len(g_object.get_bytes()) <= 4)

    def iswords(self, g_object):
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
        "w_asSymbol",
        "version",
        "startup_time",
        "space"
    ]

    def __init__(self, reader):
        space = self.space = reader.space
        self.special_objects = space.wrap_list(reader.special_w_objects)
        self.w_asSymbol = self.find_symbol(space, reader, "asSymbol")
        self.lastWindowSize = reader.lastWindowSize
        self.version = reader.version
        self.run_spy_hacks(space)
        self.startup_time = time.time()
        from spyvm.plugins.simulation import SIMULATE_PRIMITIVE_SELECTOR
        self.w_simulatePrimitive = self.find_symbol(space, reader, SIMULATE_PRIMITIVE_SELECTOR)

    def run_spy_hacks(self, space):
        if not space.run_spy_hacks.is_set():
            return
        w_display = space.objtable["w_display"]
        if w_display is not None and not w_display.is_nil(space):
            if space.unwrap_int(w_display.fetch(space, 3)) < 8:
                # non-native indexed color depth not well supported
                w_display.store(space, 3, space.wrap_int(8))

    def find_symbol(self, space, reader, symbol):
        w_dnu = self.special(constants.SO_DOES_NOT_UNDERSTAND)
        assert isinstance(w_dnu, model.W_BytesObject)
        assert space.unwrap_string(w_dnu) == "doesNotUnderstand:"
        w_Symbol = w_dnu.getclass(space)
        w_obj = None
        # bit annoying that we have to hunt through the image :-(
        for chunk in reader.chunklist:
            w_obj = chunk.g_object.w_object
            if not isinstance(w_obj, model.W_BytesObject):
                continue
            if not w_obj.getclass(space).is_same_object(w_Symbol):
                continue
            if space.unwrap_string(w_obj) == symbol:
                return w_obj
        w_obj = space.w_nil
        return w_obj

    def special(self, index):
        return self.special_objects.at0(self.space, index)

# ____________________________________________________________

class GenericObject(object):
    """ Intermediate representation of squeak objects. To establish all
        pointers as object references, ImageReader creates instances of
        GenericObject from the image chunks, and uses them as starting
        point for the actual create of spyvm.model classes.
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
        self.w_object = model.W_Character(untagged_value)
        self.filled_in = True

    def initialize(self, chunk, reader, space):
        self.reader = reader
        self.size = chunk.size
        self.hash = chunk.hash
        self.format = chunk.format
        self.chunk = chunk # for bytes, words and compiledmethod
        self.init_class()
        self.init_data(space) # for pointers
        self.w_object = None

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
            self.pointers = self.reader.decode_pointers(self, space, literalsize + 1) # adjust +1 for the header

    def init_w_object(self, space):
        if self.w_object is None:
            self.w_object = self.reader.instantiate(self)
        return self.w_object

    def isweak(self):
        return self.reader.isweak(self)

    def get_bytes(self):
        bytes = self.reader.get_bytes_of(self.chunk)
        stop = len(bytes) - (self.format & 3)
        assert stop >= 0
        return bytes[:stop] # omit odd bytes

    def get_ruints(self, required_len=-1):
        from rpython.rlib.rarithmetic import r_uint
        words = [r_uint(x) for x in self.chunk.data]
        if required_len != -1 and len(words) != required_len:
            raise error.CorruptImageError("Expected %d words, got %d" % (required_len, len(words)))
        return words

    def fillin(self, space):
        if not self.filled_in:
            self.filled_in = True
            self.w_object.fillin(space, self)
            self.reader.log_object_filledin()

    def fillin_weak(self, space):
        if not self.filled_in_weak and self.isweak():
            self.filled_in_weak = True
            self.w_object.fillin_weak(space, self)
            self.reader.log_weakobject_filledin()

    def get_g_pointers(self):
        assert self.pointers is not None
        return self.pointers

    def get_pointers(self):
        return [g_object.w_object for g_object in self.get_g_pointers()]

    def get_class(self):
        w_class = self.g_class.w_object
        assert isinstance(w_class, model.W_PointersObject)
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
