import os, time
from spyvm import constants, model, util, error
from spyvm.util import stream, system
from spyvm.util.bitmanipulation import splitter
from rpython.rlib import objectmodel

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
    
    def __init__(self, magic, is_big_endian, is_64bit, has_closures, has_floats_reversed):
        self.magic = magic
        self.is_big_endian = is_big_endian
        self.is_64bit = is_64bit
        self.has_closures = has_closures
        self.has_floats_reversed = has_floats_reversed
        self.is_modern = magic > 6502
    
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
}

image_versions_64bit = {
    # Versions for 64 bit images (expressed as two 32-bit words)
    (0x00000000,  0x000109A0): ImageVersion(68000, True,  True,  False, False),
    (-0x5ff6ff00, 0x00000000): ImageVersion(68000, False, True,  False, False), # 0xA009010000000000
    (0x00000000,  0x000109A2): ImageVersion(68002, True,  True,  True,  False),
    (-0x5df6ff00, 0x00000000): ImageVersion(68002, False, True,  True,  False), # 0xA209010000000000
    (0x00000000,  0x000109A3): ImageVersion(68003, True,  True,  True,  True ),
    (-0x5cf6ff00, 0x00000000): ImageVersion(68003, False, True,  True,  True ), # 0xA309010000000000
}

# ____________________________________________________________
#
# Parser classes for Squeak image format.

class ImageReader(object):
    
    def __init__(self, space, stream):
        self.space = space
        self.stream = stream
        self.version = None
        self.chunks = {} # Dictionary mapping old address to chunk object
        self.chunklist = [] # Flat list of all read chunks
        self.intcache = {} # Cached instances of SmallInteger
        self.lastWindowSize = 0
    
    def create_image(self):
        self.read_all()
        return SqueakImage(self)
    
    def log_progress(self, progress, char):
        if progress % 1000 == 0:
            os.write(2, char)
    
    def read_all(self):
        self.read_header()
        self.read_body()
        self.init_compactclassesarray()
        # All chunks are read, now convert them to real objects.
        self.init_g_objects()
        self.assign_prebuilt_constants()
        self.init_w_objects()
        self.fillin_w_objects()
        self.populate_special_objects()

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
    
    def read_header(self):
        self.read_version()
        # 1 word headersize
        headersize = self.stream.next()
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
        self.stream.skipbytes(headersize - self.stream.pos)
    
    def read_body(self):
        self.stream.reset_count()
        while self.stream.count < self.endofmemory:
            chunk, pos = self.read_object()
            self.log_progress(len(self.chunklist), '#')
            self.chunklist.append(chunk)
            self.chunks[pos + self.oldbaseaddress] = chunk
        self.stream.close()
        return self.chunklist # return for testing

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
        size = chunk.size
        chunk.data = [self.stream.next()
                     for _ in range(size - 1)] #size-1, excluding header
        return chunk, pos

    def read_1wordobjectheader(self):
        kind, size, format, classid, idhash = (
            splitter[2,6,4,5,12](self.stream.next()))
        assert kind == 3
        return ImageChunk(self.space, size, format, classid, idhash), self.stream.count - 4

    def read_2wordobjectheader(self):
        assert self.stream.peek() & 3 == 1 #kind
        classid = self.stream.next() - 01 # remove headertype to get pointer
        kind, size, format, _, idhash = splitter[2,6,4,5,12](self.stream.next())
        assert kind == 1
        return ImageChunk(self.space, size, format, classid, idhash), self.stream.count - 4

    def read_3wordobjectheader(self):
        kind, size = splitter[2,30](self.stream.next())
        assert kind == 0
        assert splitter[2](self.stream.peek())[0] == 0 #kind
        classid = self.stream.next() - 00 # remove headertype to get pointer
        kind, _, format, _, idhash = splitter[2,6,4,5,12](self.stream.next())
        assert kind == 0
        return ImageChunk(self.space, size, format, classid, idhash), self.stream.count - 4
    
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
    
    def init_g_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.as_g_object(self) # initialize g_object
        self.special_g_objects = self.chunks[self.specialobjectspointer].g_object.pointers

    def assign_prebuilt_constants(self):
        # Assign classes and objects that in special objects array that are already created.
        self._assign_prebuilt_constants(constants.objects_in_special_object_table, self.space.objtable)
        classtable = self.space.classtable
        if not self.version.is_modern:
            classtable = classtable.copy()
            # In non-modern images (pre 4.0), there was no BlockClosure class.
            del classtable["w_BlockClosure"]
        self._assign_prebuilt_constants(constants.classes_in_special_object_table, classtable)

    def _assign_prebuilt_constants(self, names_and_indices, prebuilt_objects):
        for name, so_index in names_and_indices.items():
            name = "w_" + name
            if name in prebuilt_objects:
                try:
                    w_object = prebuilt_objects[name]
                    g_object = self.special_object(so_index)
                    if g_object.w_object is None:
                        g_object.w_object = w_object
                    else:
                        if not g_object.w_object.is_nil(self.space):
                           raise Warning('Object found in multiple places in the special objects array')
                except IndexError:
                    # certain special objects might not yet be in the image's table
                    pass
    
    def special_object(self, index):
        # while python would raise an IndexError, after translation a nonexisting key results in a segfault...
        if index >= len(self.special_g_objects):
            raise IndexError
        return self.special_g_objects[index]
    
    def init_w_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.init_w_object()
        self.special_w_objects = [g.w_object for g in self.special_g_objects]

    def populate_special_objects(self):
        self.space.populate_special_objects(self.special_w_objects)
    
    def fillin_w_objects(self):
        self.filledin_objects = 0
        for chunk in self.chunks.itervalues():
            chunk.g_object.fillin(self.space)

    def log_object_filledin(self):
        self.filledin_objects = self.filledin_objects + 1
        self.log_progress(self.filledin_objects, '%')


# ____________________________________________________________

class SqueakImage(object):
    _immutable_fields_ = ["w_asSymbol", "w_simulateCopyBits", "version", "startup_time"]

    def __init__(self, reader):
        space = reader.space
        self.special_objects = reader.special_w_objects
        self.w_asSymbol = self.find_symbol(space, reader, "asSymbol")
        self.w_simulateCopyBits = self.find_symbol(space, reader, "simulateCopyBits")
        self.lastWindowSize = reader.lastWindowSize
        self.version = reader.version
        self.run_spy_hacks(space)
        self.startup_time = time.time()

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
        assert w_dnu.as_string() == "doesNotUnderstand:"
        w_Symbol = w_dnu.getclass(space)
        w_obj = None
        # bit annoying that we have to hunt through the image :-(
        for chunk in reader.chunklist:
            w_obj = chunk.g_object.w_object
            if not isinstance(w_obj, model.W_BytesObject):
                continue
            if not w_obj.getclass(space).is_same_object(w_Symbol):
                continue
            if w_obj.as_string() == symbol:
                break
        assert w_obj is not None
        return w_obj

    def special(self, index):
        return self.special_objects[index]

# ____________________________________________________________

class GenericObject(object):
    """ Intermediate representation of squeak objects. To establish all
        pointers as object references, ImageReader creates instances of
        GenericObject from the image chunks, and uses them as starting
        point for the actual create of spyvm.model classes.
        """

    def __init__(self, space):
        self.space = space
        self.reader = None
        self.filled_in = False

    def isinitialized(self):
        return self.reader is not None

    def initialize_int(self, value, reader):
        self.reader = reader
        self.value = value
        self.size = -1
        if value in reader.intcache:
            w_int = reader.intcache[value]
        else:
            w_int = self.space.wrap_int(value)
            reader.intcache[value] = w_int
        self.w_object = w_int
        self.filled_in = True

    def initialize(self, chunk, reader):
        self.reader = reader
        self.size = chunk.size
        self.hash12 = chunk.hash12
        self.format = chunk.format
        self.chunk = chunk # for bytes, words and compiledmethod
        self.init_class()
        self.init_data() # for pointers
        self.w_object = None

    def init_class(self):
        if self.chunk.iscompact():
            self.g_class = self.reader.compactclasses[self.chunk.classid
                - 1].g_object # Smalltalk is 1-based indexed
        else:
            self.g_class = self.reader.chunks[self.chunk.classid].g_object

    def init_data(self):
        if self.ispointers():
            self.pointers = self.decode_pointers()
            assert None not in self.pointers
        elif self.iscompiledmethod():
            header = self.chunk.data[0] >> 1 # untag tagged int
            _, literalsize, _, _, _ = constants.decode_compiled_method_header(header)
            self.pointers = self.decode_pointers(literalsize + 1) # adjust +1 for the header

    def decode_pointers(self, end=-1):
        if end == -1:
            end = len(self.chunk.data)
        pointers = []
        for i in range(end):
            pointer = self.chunk.data[i]
            if (pointer & 1) == 1:
                small_int = GenericObject(self.space)
                small_int.initialize_int(pointer >> 1, self.reader)
                pointers.append(small_int)
            else:
                pointers.append(self.reader.chunks[pointer].g_object)
        return pointers

    def isbytes(self):
        return 8 <= self.format <= 11

    def is32bitlargepositiveinteger(self):
        return (self.format == 8 and
                self.space.w_LargePositiveInteger.is_same_object(self.g_class.w_object) and
                len(self.get_bytes()) <= 4)

    def iswords(self):
        return self.format == 6

    def isfloat(self):
        return self.iswords() and self.space.w_Float.is_same_object(self.g_class.w_object)

    def ispointers(self):
        return self.format < 5

    def isweak(self):
        return self.format == 4
        
    def iscompiledmethod(self):
        return 12 <= self.format <= 15

    def init_w_object(self):
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
        if self.w_object is None:
            # the instantiate call circumvents the constructors
            # and makes empty objects
            if self.ispointers():
                self.w_object = objectmodel.instantiate(model.W_PointersObject)
            elif self.format == 5:
                raise error.CorruptImageError("Unknown format 5")
            elif self.isfloat():
                self.w_object = objectmodel.instantiate(model.W_Float)
            elif self.is32bitlargepositiveinteger():
                self.w_object = objectmodel.instantiate(model.W_LargePositiveInteger1Word)
            elif self.iswords():
                self.w_object = objectmodel.instantiate(model.W_WordsObject)
            elif self.format == 7:
                raise error.CorruptImageError("Unknown format 7, no 64-bit support yet :-)")
            elif self.isbytes():
                self.w_object = objectmodel.instantiate(model.W_BytesObject)
            elif self.iscompiledmethod():
                self.w_object = objectmodel.instantiate(model.W_CompiledMethod)
            else:
                assert 0, "not reachable"
        return self.w_object

    def get_bytes(self):
        bytes = []
        if self.reader.version.is_big_endian:
            for each in self.chunk.data:
                bytes.append(chr((each >> 24) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 0) & 0xff))
        else:
            for each in self.chunk.data:
                bytes.append(chr((each >> 0) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 24) & 0xff))
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
        return self.chunk.hash12


class ImageChunk(object):
    """ A chunk knows the information from the header, but the body of the
    object is not decoded yet."""
    def __init__(self, space, size, format, classid, hash12):
        self.size = size
        self.format = format
        self.classid = classid
        self.hash12 = hash12
        # list of integers forming the body of the object
        self.data = None
        self.g_object = GenericObject(space)

    def __eq__(self, other):
        "(for testing)"
        return (self.__class__ is other.__class__ and
                self.format == other.format and
                self.classid == other.classid and
                self.hash12 == other.hash12 and
                self.data == other.data)

    def __ne__(self, other):
        "(for testing)"
        return not self == other

    def as_g_object(self, reader):
        if not self.g_object.isinitialized():
            self.g_object.initialize(self, reader)
        return self.g_object

    def iscompact(self):
        return 0 < self.classid < 32
