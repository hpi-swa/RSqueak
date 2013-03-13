import py
import os
import sys
from spyvm import constants
from spyvm import model
from spyvm.tool.bitmanipulation import splitter

from rpython.rlib import objectmodel

def chrs2int(b):
    assert len(b) == 4
    first = ord(b[0]) # big endian
    if first & 0x80 != 0:
        first = first - 0x100
    return (first << 24 | ord(b[1]) << 16 | ord(b[2]) << 8 | ord(b[3]))

def swapped_chrs2int(b):
    assert len(b) == 4
    first = ord(b[3]) # little endian
    if first & 0x80 != 0:
        first = first - 0x100
    return (first << 24 | ord(b[2]) << 16 | ord(b[1]) << 8 | ord(b[0]))

def chrs2long(b):
    assert len(b) == 8
    first = ord(b[0]) # big endian
    if first & 0x80 != 0:
        first = first - 0x100
    return (      first << 56 | ord(b[1]) << 48 | ord(b[2]) << 40 | ord(b[3]) << 32
            | ord(b[4]) << 24 | ord(b[5]) << 16 | ord(b[6]) <<  8 | ord(b[7])      )

def swapped_chrs2long(b):
    assert len(b) == 8
    first = ord(b[7]) # little endian
    if first & 0x80 != 0:
        first = first - 0x100
    return (      first << 56 | ord(b[6]) << 48 | ord(b[5]) << 40 | ord(b[4]) << 32
            | ord(b[3]) << 24 | ord(b[2]) << 16 | ord(b[1]) <<  8 | ord(b[0])      )


# ____________________________________________________________
#
# Reads an image file and creates all model objects

class Stream(object):
    """ Simple input stream """
    def __init__(self, inputfile=None, data=None):
        if inputfile is None and data is None:
            raise RuntimeError("need to supply either inputfile or data")

        if inputfile:
            try:
                self.data = inputfile.read()
            finally:
                inputfile.close()
        else:
            self.data = data
        self.reset()

    def peek(self):
        if self.pos >= len(self.data):
            raise IndexError
        data_peek = self.data[self.pos:self.pos + self.word_size]
        if self.use_long_read:
            if self.swap:
                return swapped_chrs2long(data_peek)
            else:
                return chrs2long(data_peek)
        else:
            if self.swap:
                return swapped_chrs2int(data_peek)
            else:
                return chrs2int(data_peek)
    

    def next(self):
        integer = self.peek()
        self.pos += self.word_size
        self.count += self.word_size
        return integer

    def reset(self):
        self.swap = False
        self.pos = 0
        self.count = 0
        self.be_32bit()

    def reset_count(self):
        self.count = 0

    def skipbytes(self, jump):
        assert jump > 0
        assert (self.pos + jump) <= len(self.data)
        self.pos += jump
        self.count += jump

    def skipwords(self, jump):
        self.skipbytes(jump * self.word_size)
        assert (self.pos + jump) <= len(self.data)
        self.pos += jump
        self.count += jump


    def length(self):
        return len(self.data)

    def close(self):
        pass # already closed

    def be_64bit(self):
        self.word_size = 8
        self.use_long_read = True

    def be_32bit(self):
        self.word_size = 4
        self.use_long_read = False


class CorruptImageError(Exception):
    pass

class UnsupportedImageError(Exception):
    pass

# ____________________________________________________________

class ImageVersion(object):

    def __init__(self, magic, is_big_endian, is_64bit, has_closures, has_floats_reversed):
        self.magic = magic
        self.is_big_endian = is_big_endian
        self.is_64bit = is_64bit
        self.has_closures = has_closures
        self.has_floats_reversed = has_floats_reversed

image_versions = {
    0x00001966:         ImageVersion(6502,  True,  False, False, False),
    0x66190000:         ImageVersion(6502,  False, False, False, False),
    0x00001968:         ImageVersion(6504,  True,  False, True,  False),
    0x68190000:         ImageVersion(6504,  False, False, True,  False),
    0x00001969:         ImageVersion(6505,  True,  False, True,  True ),
    0x69190000:         ImageVersion(6505,  False, False, True,  True ),
    0x00000000000109A0: ImageVersion(68000, True,  True,  False, False),
}

if sys.maxint == 2 ** 63 - 1:
    image_versions.update({
   -0x5ff6ff0000000000:
    # signed version of 0xA009010000000000: 
                        ImageVersion(68000, False, True,  False, False),
    0x00000000000109A2: ImageVersion(68002, True,  True,  True,  False),
   -0x5df6ff0000000000:
    # signed version of 0xA209010000000000: 
			ImageVersion(68002, False, True,  True,  False),
    0x00000000000109A3: ImageVersion(68003, True,  True,  True,  True ),
   -0x5cf6ff0000000000:
    # signed version of 0xA309010000000000: 
			ImageVersion(68003, False, True,  True,  True ),
})

    
def version(magic):
    ver = image_versions.get(magic, None)
    if ver is None:
        raise CorruptImageError
    # if ver.is_64bit or ver.has_floats_reversed:
    #     raise UnsupportedImageError
    return ver

possible_image_offset = 512

def version_from_stream(stream):
    # 32 bit
    try:
        return version(stream.peek())
    except CorruptImageError as e:
        if stream.length() > possible_image_offset + 4:
            stream.skipbytes(possible_image_offset)
            try:
                return version(stream.peek())
            except CorruptImageError:
                pass # raise original error
        # 64 bit
        stream.reset()
        stream.be_64bit()
        try:
            v = version(stream.peek())
            assert v.is_64bit
            return v
        except CorruptImageError as e:
            if stream.length() > possible_image_offset + 4:
                stream.skipbytes(possible_image_offset)
                try:
                    v = version(stream.peek())
                    assert v.is_64bit
                    return v
                except CorruptImageError:
                    pass # raise original error
        raise

    
    
def reader_for_image(space, stream):
    ver = version_from_stream(stream)
    if not ver.is_big_endian:
        stream.swap = True
    return ImageReader(space, stream, ver)

class ImageReader(object):
    
    def __init__(self, space, stream, version):
        self.space = space
        self.stream = stream
        self.version = version
        # dictionary mapping old address to chunk object
        self.chunks = {}
        self.chunklist = []
        # cache wrapper integers
        self.intcache = {}

    def initialize(self):
        # XXX should be called something like read_full_image
        self.read_header()
        self.read_body()
        self.init_compactclassesarray()
        # until here, the chunks are generated
        self.init_g_objects()
        self.init_w_objects()
        self.fillin_w_objects()
        self.synchronize_shadows()

    def read_version(self):
        # 1 word version
        magic = self.stream.next()
        assert self.version.magic == magic

    def read_header(self):
        self.read_version()
        #------
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
        savedwindowssize = self.stream.next()
        print "savedwindowssize", savedwindowssize
        fullscreenflag = self.stream.next()
        extravmmemory = self.stream.next()
        self.stream.skipbytes(headersize - self.stream.pos)

    def read_body(self):
        import sys
        self.stream.reset_count()
        while self.stream.count < self.endofmemory:
            chunk, pos = self.read_object()
            if len(self.chunklist) % 1000 == 0: os.write(2,'#')
            self.chunklist.append(chunk)
            self.chunks[pos + self.oldbaseaddress] = chunk
        self.stream.close()
        self.swap = self.stream.swap #save for later
        self.stream = None
        return self.chunklist # return for testing

    def init_g_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.as_g_object(self) # initialized g_object

    def init_w_objects(self):
        self.assign_prebuilt_constants()
        for chunk in self.chunks.itervalues():
            chunk.g_object.init_w_object()

    def assign_prebuilt_constants(self):
        # assign w_objects for objects that are already in objtable
        for name, so_index in constants.objects_in_special_object_table.items():
            w_object = self.space.objtable["w_" + name]
            if self.special_object(so_index).w_object is None:
                self.special_object(so_index).w_object = w_object
            else:
                if self.special_object(0).w_object is not self.space.w_nil:
                   raise Warning('Object found in multiple places in the special objects array')
        # assign w_objects for objects that are already in classtable
        for name, so_index in constants.classes_in_special_object_table.items():
            w_object = self.space.classtable["w_" + name]
            if self.special_object(so_index).w_object is None:
                self.special_object(so_index).w_object = w_object
            else:
                if self.special_object(0).w_object is not self.space.w_nil:
                   raise Warning('Object found in multiple places in the special objects array')

    def special_object(self, index):
        special = self.chunks[self.specialobjectspointer].g_object.pointers
        return special[index]

    def fillin_w_objects(self):
        for chunk in self.chunks.itervalues():
            chunk.g_object.fillin_w_object()

    def synchronize_shadows(self):
        for chunk in self.chunks.itervalues():
            casted = chunk.g_object.w_object
            if isinstance(casted, model.W_PointersObject) and casted.has_shadow():
                casted._shadow.update()

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

    def read_object(self):
        kind = self.stream.peek() & 3 # 2 bits
        if kind == 0: # 00 bits
            chunk, pos = self.read_3wordobjectheader()
        elif kind == 1: # 01 bits
            chunk, pos = self.read_2wordobjectheader()
        elif kind == 3: # 11 bits
            chunk, pos = self.read_1wordobjectheader()
        else: # 10 bits
            raise CorruptImageError("Unused block not allowed in image")
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


# ____________________________________________________________

class SqueakImage(object):

    def from_reader(self, space, reader):
        from spyvm import constants
        self.special_objects = [g_object.w_object for g_object in
                                reader.chunks[reader.specialobjectspointer]
                                .g_object.pointers]

        for name, idx in constants.objects_in_special_object_table.items():
            space.objtable["w_" + name] = self.special_objects[idx]

        self.w_asSymbol = self.find_asSymbol(space, reader)

    def find_asSymbol(self, space, reader):
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
            if w_obj.as_string() == "asSymbol":
                break
        assert w_obj is not None
        return w_obj

    def special(self, index):
        return self.special_objects[index]

# from the squeak source code:
# in squeak, the compact classes array can be found at this position
# in the special objects array
COMPACT_CLASSES_ARRAY = 28

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

    def initialize(self, chunk, reader):
        self.reader = reader
        self.size = chunk.size
        self.hash12 = chunk.hash12
        self.format = chunk.format
        self.init_class(chunk)
        self.init_data(chunk) # for pointers
        self.chunk = chunk # for bytes, words and compiledmethod
        self.w_object = None

    def init_class(self, chunk):
        if chunk.iscompact():
            self.g_class = self.reader.compactclasses[chunk.classid
                - 1].g_object # Smalltalk is 1-based indexed
        else:
            self.g_class = self.reader.chunks[chunk.classid].g_object

    def init_data(self, chunk):
        if not self.ispointers(): return
        self.pointers = [self.decode_pointer(pointer)
                         for pointer in chunk.data]
        assert None not in self.pointers

    def decode_pointer(self, pointer):
        if (pointer & 1) == 1:
            small_int = GenericObject(self.space)
            small_int.initialize_int(pointer >> 1, self.reader)
            return small_int
        else:
            return self.reader.chunks[pointer].g_object

    def isbytes(self):
        return 8 <= self.format <= 11

    def iswords(self):
        return self.format == 6

    def isfloat(self):
        return self.iswords() and self.space.w_Float.is_same_object(self.g_class.w_object)

    def ispointers(self):
        return self.format < 5 #TODO, what about compiled methods?

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
                # XXX self.format == 4 is weak
                self.w_object = objectmodel.instantiate(model.W_PointersObject)
            elif self.format == 5:
                raise CorruptImageError("Unknown format 5")
            elif self.isfloat():
                self.w_object = objectmodel.instantiate(model.W_Float)
            elif self.iswords():
                self.w_object = objectmodel.instantiate(model.W_WordsObject)
            elif self.format == 7:
                raise CorruptImageError("Unknown format 7, no 64-bit support yet :-)")
            elif self.isbytes():
                self.w_object = objectmodel.instantiate(model.W_BytesObject)
            elif self.iscompiledmethod():
                self.w_object = objectmodel.instantiate(model.W_CompiledMethod)
            else:
                assert 0, "not reachable"
        self.w_object.space = self.space
        return self.w_object

    def fillin_w_object(self):
        # below we are using an RPython idiom to 'cast' self.w_object
        # and pass the casted reference to the fillin_* methods
        casted = self.w_object
        if isinstance(casted, model.W_PointersObject):
            self.fillin_pointersobject(casted)
        elif isinstance(casted, model.W_Float):
            self.fillin_floatobject(casted)
        elif isinstance(casted, model.W_WordsObject):
            self.fillin_wordsobject(casted)
        elif isinstance(casted, model.W_BytesObject):
            self.fillin_bytesobject(casted)
        elif isinstance(casted, model.W_CompiledMethod):
            self.fillin_compiledmethod(casted)
        else:
            assert 0
        if not objectmodel.we_are_translated():
            assert casted.invariant()

    def fillin_pointersobject(self, w_pointersobject):
        assert self.pointers is not None
        w_pointersobject._vars = [g_object.w_object for g_object in self.pointers]
        w_class = self.g_class.w_object
        assert isinstance(w_class, model.W_PointersObject)
        w_pointersobject.w_class = w_class
        w_pointersobject.s_class = None
        w_pointersobject.hash = self.chunk.hash12

    def fillin_floatobject(self, w_floatobject):
        from rpython.rlib.rarithmetic import r_uint
        words = [r_uint(x) for x in self.chunk.data]
        if len(words) != 2:
            raise CorruptImageError("Expected 2 words in Float, got %d" % len(words))
        w_class = self.g_class.w_object
        assert isinstance(w_class, model.W_PointersObject)
        w_floatobject.fillin_fromwords(self.space, words[0], words[1])

    def fillin_wordsobject(self, w_wordsobject):
        from rpython.rlib.rarithmetic import r_uint
        w_wordsobject.words = [r_uint(x) for x in self.chunk.data]
        w_class = self.g_class.w_object
        assert isinstance(w_class, model.W_PointersObject)
        w_wordsobject.w_class = w_class
        w_wordsobject.hash = self.chunk.hash12 # XXX check this

    def fillin_bytesobject(self, w_bytesobject):
        w_class = self.g_class.w_object
        assert isinstance(w_class, model.W_PointersObject)
        w_bytesobject.w_class = w_class
        w_bytesobject.bytes = self.get_bytes()
        w_bytesobject.hash = self.chunk.hash12 # XXX check this

    def get_bytes(self):
        bytes = []
        if self.reader.swap:
            for each in self.chunk.data:
                bytes.append(chr((each >> 0) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 24) & 0xff))
        else:
            for each in self.chunk.data:
                bytes.append(chr((each >> 24) & 0xff))
                bytes.append(chr((each >> 16) & 0xff))
                bytes.append(chr((each >> 8) & 0xff))
                bytes.append(chr((each >> 0) & 0xff))
        #strange, for example range(4)[:0] returns [] instead of [0,1,2,3]!
        #hence what we have to write list[:-odd] as list[:len(list)-odd] instead :(
        stop = len(bytes)-(self.format & 3)
        assert stop >= 0
        return bytes[:stop] # omit odd bytes

    def fillin_compiledmethod(self, w_compiledmethod):
        header = self.chunk.data[0]
        w_compiledmethod.setheader(header>>1) # We untag before giving header
        for i in range(1,w_compiledmethod.literalsize+1):
            w_compiledmethod.literalatput0(
                self.space, i, self.decode_pointer(self.chunk.data[i]).w_object)
        bbytes = self.get_bytes()[(w_compiledmethod.literalsize + 1)*4:]
        w_compiledmethod.setbytes(bbytes)

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


