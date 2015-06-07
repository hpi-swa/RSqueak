# -*- coding: utf-8 -*-
import py, StringIO
from struct import pack, unpack
from spyvm import squeakimage, error
from spyvm import model
from spyvm.util.stream import chrs2int, chrs2long, swapped_chrs2long
from .util import create_space

# ----- helpers ----------------------------------------------

def ints2str(*ints):
    return pack(">" + "I" * len(ints), *ints)

def longs2str(*longs):
    return pack(">" + "Q" * len(longs), *longs)

def joinbits(values, lengths):
    result = 0
    for each, length in reversed(zip(values, lengths)):
        result = result << length
        result += each
    return result   

def imagestream_mock(string):
    f = StringIO.StringIO(string)
    return squeakimage.Stream(inputfile=f)

def imagereader_mock(string):
    stream = imagestream_mock(string)
    return squeakimage.ImageReader(create_space(), stream)

SIMPLE_VERSION_HEADER = pack(">i", 6502)
SIMPLE_VERSION_HEADER_LE = pack("<i", 6502)
SPUR_VERSION_HEADER = pack(">i", 6521)
SPUR_VERSION_HEADER_LE = pack("<i", 6521)

# ----- tests ------------------------------------------------

def test_chrs2int():
    assert 1 == chrs2int('\x00\x00\x00\x01')
    assert -1 == chrs2int('\xFF\xFF\xFF\xFF')

def test_chrs2long():
    assert 1 == chrs2long('\x00\x00\x00\x00\x00\x00\x00\x01')
    assert -1 == chrs2long('\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF')
    assert 68002 == chrs2long(pack(">Q", 68002))
    assert 68002 == swapped_chrs2long(pack("<Q", 68002))

def test_stream():
    stream = imagestream_mock(SIMPLE_VERSION_HEADER)
    n = stream.peek()
    assert n == 6502 
    n = stream.next()
    assert n == 6502 
    py.test.raises(IndexError, lambda: stream.next())
    
def test_stream_little_endian():
    stream = imagestream_mock('\x66\x19\x00\x00')
    stream.big_endian = False
    first = stream.next()
    assert first == 6502 
    py.test.raises(IndexError, lambda: stream.next())
    
def test_stream_many():
    stream = imagestream_mock(SIMPLE_VERSION_HEADER * 5)
    for each in range(5):
        first = stream.peek()
        assert first == 6502 
        value = stream.next()
        assert value == 6502 
    py.test.raises(IndexError, lambda: stream.next())
    
def test_stream_skipbytes():
    stream = imagestream_mock('\xFF\xFF\xFF' + SIMPLE_VERSION_HEADER)
    stream.skipbytes(3)
    value = stream.next()
    assert value == 6502 
    py.test.raises(IndexError, lambda: stream.next())
        
def test_stream_count():
    stream = imagestream_mock('\xFF' * 20)
    stream.next()
    stream.next()
    stream.reset_count()
    assert stream.count == 0
    stream.next()        
    assert stream.count == 4
    stream.next()        
    assert stream.count == 8

def test_stream_next_short():
    s = imagestream_mock('\x01\x02\x03\x04\x05\x06\x07\x08')
    s.be_32bit()
    assert s.next_short() == 0x0102
    assert s.next_short() == 0x0304
    assert s.next() == 0x05060708

def test_stream_next_short_64b():
    s = imagestream_mock('\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c')
    s.be_64bit()
    assert s.next_short() == 0x0102
    assert s.next_short() == 0x0304
    assert s.next() == 0x05060708090a0b0c
    
   
def test_simple_joinbits():
    assert 0x01010101 == joinbits(([1] * 4), [8,8,8,8])
    assert 0xFfFfFfFf == joinbits([255] * 4, [8,8,8,8])
    
def test_fancy_joinbits():    
    assert 0x01020304 == joinbits([4,3,2,1], [8,8,8,8])
    assert 0x3Ff == joinbits([1,3,7,15], [1,2,3,4])
    
    
def test_ints2str():
    assert "\x00\x00\x00\x02" == ints2str(2)       
    assert SIMPLE_VERSION_HEADER + '\x00\x00\x00\x02' == ints2str(6502,2)
    
def test_freeblock():
    r = imagereader_mock(SIMPLE_VERSION_HEADER + "\x00\x00\x00\x02")
    r.read_version()
    py.test.raises(error.CorruptImageError, lambda: r.readerStrategy.read_object())

def test_1wordobjectheader():
    s = ints2str(joinbits([3, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(1, 2, 3, 4), 0 + l) == r.readerStrategy.read_1wordobjectheader()

def test_1wordobjectheader2():
    s = ints2str(joinbits([3, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + (s * 3))
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(1, 2, 3, 4), 0 + l) == r.readerStrategy.read_1wordobjectheader()
    assert (squeakimage.ImageChunk(1, 2, 3, 4), 4 + l) == r.readerStrategy.read_1wordobjectheader()
    assert (squeakimage.ImageChunk(1, 2, 3, 4), 8 + l) == r.readerStrategy.read_1wordobjectheader()

def test_2wordobjectheader():
    s = ints2str(4200 + 1, joinbits([1, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(1, 2, 4200, 4), 4 + l) == r.readerStrategy.read_2wordobjectheader()

def test_3wordobjectheader():
    s = ints2str(1701 << 2, 4200 + 0, joinbits([0, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(1701, 2, 4200, 4), 8 + l) == r.readerStrategy.read_3wordobjectheader()
    
def test_read3wordheaderobject():
    size = 42
    s = ints2str(size << 2, 4200 + 0, joinbits([0, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s + SIMPLE_VERSION_HEADER * (size - 1))
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    chunk, pos = r.readerStrategy.read_object()
    chunk0 = squeakimage.ImageChunk(size, 2, 4200, 4)
    chunk0.data = [6502] * (size - 1)
    assert pos == 8 + l
    assert chunk0 == chunk

def test_object_format_v3(monkeypatch):
    g_class_mock = squeakimage.GenericObject()
    from rpython.rlib import objectmodel
    w_class_mock = objectmodel.instantiate(model.W_PointersObject)
    g_class_mock.w_object = w_class_mock
    def assert_w_object_type(format, expected_type, length=0,
            compact_class_index=0, body="", assert_is_weak=False):
        objbytes = ints2str(joinbits([3, length + 1, format, compact_class_index, 0],
                              [2,6,4,5,12])) + body
        r = imagereader_mock(SIMPLE_VERSION_HEADER + objbytes)
        r.read_version()
        monkeypatch.setattr(r.readerStrategy, 'g_class_of',
                lambda chunk: g_class_mock)
        chunk, pos = r.readerStrategy.read_object()
        g_object = squeakimage.GenericObject()
        g_object.initialize(chunk, r.readerStrategy, r.space)
        w_object = g_object.init_w_object(r.space)
        g_object.fillin(r.space)
        assert w_object is g_object.w_object
        assert isinstance(w_object, expected_type)
        if assert_is_weak:
            assert w_object.is_weak()
        return w_object, r.space
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
    w_obj, _ = assert_w_object_type(0, model.W_PointersObject)
    assert w_obj.size() == 0
    body_42_and_1 = ints2str(joinbits([1, 42], [1, 31]), joinbits([1, 1], [1, 31]))
    w_obj, space = assert_w_object_type(1, model.W_PointersObject, length=2, body=body_42_and_1)
    assert w_obj.size() == 2
    assert space.unwrap_int(w_obj.fetch(space, 0)) == 42
    assert space.unwrap_int(w_obj.fetch(space, 1)) == 1
    w_obj, space = assert_w_object_type(2, model.W_PointersObject, length=2,
            body=body_42_and_1)
    assert w_obj.size() == 2
    assert space.unwrap_int(w_obj.fetch(space, 0)) == 42
    assert space.unwrap_int(w_obj.fetch(space, 1)) == 1
    assert_w_object_type(3, model.W_PointersObject, length=2, body=body_42_and_1)
    w_obj, _ = assert_w_object_type(4, model.W_PointersObject, length=2,
            body=body_42_and_1)
    assert w_obj.is_weak()
    assert w_obj.size() == 2
    assert space.unwrap_int(w_obj.fetch(space, 0)) == 42
    assert space.unwrap_int(w_obj.fetch(space, 1)) == 1
    w_obj, space = assert_w_object_type(6, model.W_WordsObject, length=2,
            body=body_42_and_1)
    assert w_obj.size() == 2
    assert w_obj.getword(0) == 42 << 1 | 1
    assert w_obj.getword(1) == 1 << 1 | 1
    w_obj, space = assert_w_object_type(8, model.W_BytesObject, length=2,
            body=body_42_and_1)
    assert w_obj.size() == 8 # 2 * 32 bit == 8 * 8 bit
    assert w_obj.as_string() == body_42_and_1
    w_obj, space = assert_w_object_type(12, model.W_CompiledMethod, length=2,
            body=body_42_and_1)
    assert w_obj.size() == 8

def test_read_normal_spur_header():
    # Array of pointers
    n_slots = 42
    objbytes = ints2str(joinbits([48, 0, n_slots], [22, 2, 8]),
            joinbits([10, 0, 2, 0], [22, 2, 5, 3])) + ints2str(0) * n_slots
    r = imagereader_mock(SPUR_VERSION_HEADER + objbytes)
    r.read_version()
    r.stream.reset_count()
    actualChunk, pos = r.readerStrategy.read_object()
    expectedChunk = squeakimage.ImageChunk(size=n_slots, format=2, classid=10,
            hash=48, data=[0] * n_slots)
    assert expectedChunk == actualChunk
    assert pos == 0

def test_read_long_spur_header():
    n_slots = 3000
    objbytes = longs2str(joinbits([n_slots, 255], [56, 8])) + ints2str(
            joinbits([55, 0, 255], [22, 2, 8]),
            joinbits([10, 0, 2, 0], [22, 2, 5, 3])) + ints2str(0) * n_slots
    r = imagereader_mock(SPUR_VERSION_HEADER + objbytes)
    r.read_version()
    r.stream.reset_count()
    actualChunk, pos = r.readerStrategy.read_object()
    expectedChunk = squeakimage.ImageChunk(size=n_slots, format=2, classid=10,
            hash=55, data=[0] * n_slots)
    assert expectedChunk == actualChunk
    assert pos == 8

def test_object_format_spur(monkeypatch):
    g_class_mock = squeakimage.GenericObject()
    from rpython.rlib import objectmodel
    w_class_mock = objectmodel.instantiate(model.W_PointersObject)
    g_class_mock.w_object = w_class_mock
    def assert_w_object_type(format, expected_type, length=0, classid=0, body=""):
        objbytes = ints2str(joinbits([0, 0, length], [22, 2, 8]),
                joinbits([classid, 0, format, 0], [22, 2, 5, 3])) + body
        r = imagereader_mock(SPUR_VERSION_HEADER + objbytes)
        r.read_version()
        monkeypatch.setattr(r.readerStrategy, 'g_class_of',
                lambda chunk: g_class_mock)
        r.stream.reset_count()
        chunk, pos = r.readerStrategy.read_object()
        g_object = squeakimage.GenericObject()
        g_object.initialize(chunk, r.readerStrategy, r.space)
        w_object = g_object.init_w_object(r.space)
        g_object.fillin(r.space)
        assert w_object is g_object.w_object
        assert isinstance(w_object, expected_type)
        return w_object, r.space
    # 0 zero sized object
    w_obj, space = assert_w_object_type(0, model.W_PointersObject, body=("\x00"*3+"\x01")*2)
    # 1 fixed-size object with inst-vars
    body_1_to_9 = ints2str(*(joinbits([1, n], [1, 31]) for n in range(1, 10)))
    w_obj, space = assert_w_object_type(1, model.W_PointersObject, length=2, body=body_1_to_9)
    assert w_obj.size() == 2
    assert w_obj.fetch(space, 0) == space.wrap_int(1)
    assert w_obj.fetch(space, 1) == space.wrap_int(2)
    # 2 variable sized object without inst vars
    w_obj, space = assert_w_object_type(2, model.W_PointersObject, length=2, body=body_1_to_9)
    assert w_obj.size() == 2
    assert w_obj.fetch(space, 0) == space.wrap_int(1)
    assert w_obj.fetch(space, 1) == space.wrap_int(2)
    # 3 variable sized object with inst vars
    w_obj, space = assert_w_object_type(3, model.W_PointersObject, length=2, body=body_1_to_9)
    assert w_obj.size() == 2
    assert w_obj.fetch(space, 0) == space.wrap_int(1)
    assert w_obj.fetch(space, 1) == space.wrap_int(2)
    # 4 weak variable sized object with inst vars
    w_obj, space = assert_w_object_type(4, model.W_PointersObject, length=2, body=body_1_to_9)
    assert w_obj.is_weak()
    assert w_obj.size() == 2
    assert w_obj.fetch(space, 0) == space.wrap_int(1)
    assert w_obj.fetch(space, 1) == space.wrap_int(2)
    # 5 weak fixed sized object with inst vars (Ephemeron)
    w_obj, space = assert_w_object_type(5, model.W_PointersObject, length=2, body=body_1_to_9)
    assert w_obj.is_weak()
    assert w_obj.size() == 2
    assert w_obj.fetch(space, 0) == space.wrap_int(1)
    assert w_obj.fetch(space, 1) == space.wrap_int(2)
    # 6 unused
    # 7 forwarding object, does not occur in images
    # 8 unused
    # 9 64 bit indexables
    w_obj, space = assert_w_object_type(9, model.W_WordsObject, length=2, body=body_1_to_9)
    # 64-bit not supported yet
    # assert w_obj.getlong(space, 0) == chrs2long(body_1_to_9[:8])
    # 10-11 32 bit indexables (11 unused in 32 bits)
    w_obj, space = assert_w_object_type(10, model.W_WordsObject, length=2, body=body_1_to_9)
    assert w_obj.size() == 2
    assert w_obj.getword(0) == chrs2int(body_1_to_9[:4])
    assert w_obj.getword(1) == chrs2int(body_1_to_9[4:8])
    # TODO: add tests for correct reading of 32 bit indexables with trailing slots (11)
    # 12-15 16 bit indexables (14,15 unused in 32 bits)
    w_obj, space = assert_w_object_type(12, model.W_WordsObject, length=2, body=body_1_to_9)
    # XXX assert w_obj.size() == 4
    # XXX assert w_obj.getword(0) == chrs2int("\x00\x00" + body_1_to_9[:2])
    # XXX assert w_obj.getword(1) == chrs2int("\x00\x00" + body_1_to_9[2:4])
    # TODO: add tests for correct reading of 16 bit indexables with trailing slots (13-15)
    # 16-23 8 bit indexables (20-23 unused in 32 bits)
    w_obj, space = assert_w_object_type(18, model.W_BytesObject, length=2, body=body_1_to_9)
    assert w_obj.size() == 6
    assert w_obj.as_string() == body_1_to_9[:6]
    # TODO: add tests for correct reading of 8 bit indexables with trailing slots (17-23)
    # 24-31 compiled methods (28-31 unused in 32 bits)
    w_obj, space = assert_w_object_type(24, model.W_CompiledMethod, length=2, body=body_1_to_9)
    # TODO: add tests for correct reading of compiled methods with trailing slots (25-31)

    
def test_simple_image():
    word_size = 4
    header_size = 16 * word_size

    image_1 = (SIMPLE_VERSION_HEADER     # 1
               + pack(">i", header_size) # 2 64 byte header
               + pack(">i", 0)           # 3 no body
               + pack(">i", 0)           # 4 old base addresss unset
               + pack(">i", 0)           # 5 no spl objs array
               + "\x12\x34\x56\x78"      # 6 last hash
               + pack(">h", 480)         # 7 window 480 height
               +     pack(">h", 640)     #   window 640 width
               + pack(">i", 0)           # 8 not fullscreen
               + pack(">i", 0)           # 9 no extra memory
               + ("\x00" * (header_size - (9 * word_size))))
    r = imagereader_mock(image_1)
    # does not raise
    r.read_header()
    assert r.stream.pos == len(image_1)

    image_2 = (SIMPLE_VERSION_HEADER_LE  # 1
               + pack("<i", header_size) # 2 64 byte header
               + pack("<i", 0)           # 3 no body
               + pack("<i", 0)           # 4 old base addresss unset
               + pack("<i", 0)           # 5 no spl objs array
               + "\x12\x34\x56\x78"      # 6 last hash
               + pack("<h", 480)         # 7 window 480 height
               +     pack("<h", 640)     #   window 640 width
               + pack("<i", 0)           # 8 not fullscreen
               + pack("<i", 0)           # 9 no extra memory
               + ("\x00" * (header_size - (9 * word_size))))
    r = imagereader_mock(image_2)
    # does not raise
    r.read_header()
    assert r.stream.pos == len(image_2)

def test_simple_image64(monkeypatch):
    from spyvm.util import system
    monkeypatch.setattr(system, "IS_64BIT", True)
    
    word_size = 8
    header_size = 16 * word_size

    image_1 = (pack(">Q", 68002)         # 1 version
               + pack(">q", header_size) # 2 64 byte header
               + pack(">q", 0)           # 3 no body
               + pack(">q", 0)           # 4 old base addresss unset
               + pack(">q", 0)           # 5 no spl objs array
               + ("\x12\x34\x56\x78" * 2)# 6 last hash
               + pack(">H", 480)         # 7 window 480 height
               +     pack(">H", 640)     #   window 640 width
               +     pack(">i", 0)       #   pad
               + pack(">q", 0)           # 8 not fullscreen
               + pack(">q", 0)           # 9 no extra memory
               + ("\x00" * (header_size - (9 * word_size))))
    r = imagereader_mock(image_1)
    # does not raise
    r.read_header()
    assert r.stream.pos == len(image_1)

    image_2 = (pack("<Q", 68002)         # 1 version
               + pack("<q", header_size) # 2 64 byte header
               + pack("<q", 0)           # 3 no body
               + pack("<q", 0)           # 4 old base addresss unset
               + pack("<q", 0)           # 5 no spl objs array
               + ("\x12\x34\x56\x78" * 2)# 6 last hash
               + pack("<H", 480)         # 7 window 480 height
               +     pack("<H", 640)     #   window 640 width
               +     pack(">i", 0)       #   pad
               + pack(">q", 0)           # 8 not fullscreen
               + pack("<q", 0)           # 9 no extra memory
               + ("\x00" * (header_size - (9 * word_size))))
    r = imagereader_mock(image_2)
    # does not raise
    r.read_header()
    assert r.stream.pos == len(image_2)

def spur_hdr_qword(n_slots, hash, format, classid):
    return joinbits([classid, 0, format, 0, hash, 0, n_slots], [22,2,5,3,22,2,8])

def spur_hdr_big_endian(n_slots, hash, format, classid):
    return pack(">Q", spur_hdr_qword(n_slots, hash, format, classid))

def spur_hdr_little_endian(n_slots, hash, format, classid):
    return pack("<Q", spur_hdr_qword(n_slots, hash, format, classid))

def test_spur_hdr_big_endian():
    assert "\x01\x02\x0a\x0b\x1f\x03\x0a\x0b" == spur_hdr_big_endian(
            n_slots=1, hash=(2 << 16) + (10 << 8) + 11,
            format=31, classid=(3 << 16) + (10 << 8) + 11)

def test_spur_hdr_little_endian():
    assert "\x12\x00\x00\x0A\x00\x00\x00\x20" == spur_hdr_little_endian(
            n_slots=32, hash=0, format=10, classid=18)
    assert "\x01\x02\x0a\x0b\x1f\x03\x0a\x0b"[::-1] == spur_hdr_little_endian(
            n_slots=1, hash=(2 << 16) + (0xa << 8) + 0xb,
            format=0x1f, classid=(3 << 16) + (0xa << 8) + 0xb)

def pack_be(fmt, *args):
    return pack(">" + fmt, *args)

def pack_le(fmt, *args):
    return pack("<" + fmt, *args)

def simple_spur_image(pack, spur_hdr, version):
    invalid_ptr = 12
    first_segment = (spur_hdr(0, 1000, 0, 0)   #   0 nil
                     + pack("q", 0)            #   8 body of nil
                     + spur_hdr(0, 1016, 0, 0) #  16 false
                     + pack("q", 0)            #  24 body of false
                     + spur_hdr(0, 1032, 0, 0) #  32 true
                     + pack("q", 0)            #  40 body of true
                     + spur_hdr(0, 1048, 0, 0) #  48 freeList
                     + pack("q", 0)            #  56 body of freeList
                     + spur_hdr(1, 1064, 4, 0) #  64 hiddenRoots
                     + pack("i", 80)           #  72 ptr to 1st class table page
                     + pack("i", invalid_ptr)  #  76 alignment, do not resolve
                     + spur_hdr(4, 1080, 4, 0) #  80 1st class table page
                     + pack("i", 136)          #  88 ptr to first class (here SmallInteger)
                     + pack("i", 152)          #  92 ptr to SmallInteger class
                     + pack("i", 168)          #  96 ptr to Metaclass
                     + pack("i", 184)          # 100 ptr to Metaclass class
                     + spur_hdr(6, 1104, 4, 0) # 104 special objects array
                     + pack("i", 0)            # 112 ptr to nil
                     + pack("i", 16)           # 116 ... false
                     + pack("i", 32)           # 120 ... true
                     + pack("i", 0)            # 124 ... schedulerassocptr
                     + pack("i", 0)            # 128 ... Bitmap
                     + pack("i", 136)          # 132 ... SmallInteger
                     # "arbitrary" objects from here on
                     + spur_hdr(0, 0, 0, 1)    # 136 SmallInteger (class instance)
                     + pack("q", 0)            # 144 body of SmallInteger
                     + spur_hdr(0, 1, 0, 2)    # 152 SmallInteger class
                     + pack("q", 0)            # 160 body of SmallInteger class
                     + spur_hdr(0, 2, 0, 3)    # 168 Metaclass (class instance)
                     + pack("q", 0)            # 176 body of Metaclass
                     + spur_hdr(0, 3, 0, 2)    # 184 Metaclass class
                     + pack("q", 0)            # 192 body of Metaclass class
                     + longs2str(0, 0))        # 200 final bridge = stop at 136
    body = first_segment
    word_size = 4
    header_size = 16 * word_size
    return (version                   # 1
            + pack("i", header_size)  # 2 64 byte header
            + pack("i", len(body))    # 3 body length
            + pack("i", 0)            # 4 old base addresss unset
            + pack("i", 104)          # 5 ptr to special objects array
            + "\x12\x34\x56\x78"      # 6 last hash
            + pack("h", 480)          # 7 window 480 height
            +     pack("h", 640)      #   window 640 width
            + pack("i", 0)            # 8 not fullscreen
            + pack("i", 0)            # 9 no extra memory
            + pack("h", 0)            # 10 #stack pages
            + pack("h", 0)            #    cog code size
            + pack("i", 0)            # 11 eden bytes
            + pack("h", 0)            # 12 max ext sem tab size (?)
            + pack("h", 0)            #    unused
            + pack("i", len(first_segment))  # 13 first segment size
            + pack("i", 0)            # 14 free old space in image
            + ("\x00" * (header_size - (14 * word_size)))
            + body)

def test_simple_spur_image():
    image = simple_spur_image(pack_be, spur_hdr_big_endian, SPUR_VERSION_HEADER)
    r = imagereader_mock(image)
    r.read_all() # does not raise
    assert r.stream.pos == len(image)

def test_simple_spur_image_little_endian():
    image_le = simple_spur_image(pack_le, spur_hdr_little_endian, SPUR_VERSION_HEADER_LE)
    r = imagereader_mock(image_le)
    r.read_all() # does not raise
    assert r.stream.pos == len(image_le)

def test_simple_spur_image_with_segments():
    spur_hdr = spur_hdr_big_endian
    word_size = 4
    # first segment
    # use 3000 + x as hash for debugging purposes (easier to identify g_objects)
    first_segment = (spur_hdr(0, 3000, 0, 0)   #   0 nil
                     + pack(">q", 0)           #   8 body of nil
                     + spur_hdr(0, 3016, 0, 0) #  16 false
                     + pack(">q", 0)           #  24 body of false
                     + spur_hdr(0, 3032, 0, 0) #  32 true
                     + pack(">q", 0)           #  40 body of true
                     + spur_hdr(0, 3048, 0, 0) #  48 freeList
                     + pack(">q", 0)           #  56 body of freeList
                     + spur_hdr(1, 3064, 4, 0) #  64 hiddenRoots
                     + pack(">i", 80)       #  72 ptr to 1st class table page
                     + pack(">i", 0)        #  76 8-byte alignment
                     + spur_hdr(5, 3080, 4, 0) #  80 1st class table page
                     # note that the following classtable does not match up with
                     # a "real world" spur image, the order is synthetic
                     + pack(">i", 144)      #  88 ptr to first class (here SmallInteger)
                     + pack(">i", 160)      #  92 ptr to SmallInteger class
                     + pack(">i", 176)      #  96 ptr to Metaclass
                     + pack(">i", 192)      # 100 ptr to Metaclass class
                     + pack(">i", 208)      # 104 ptr to Array
                     + pack(">i", 0)        # 108 8-byte alignment
                     + spur_hdr(6, 3112, 4, 0) # 112 special objects array
                     + pack(">i", 0)        # 120 ptr to nil
                     + pack(">i", 16)       # 124 -> false
                     + pack(">i", 32)       # 128 -> true
                     + pack(">i", 1000)     # 132 -> alienated scheduler association for this test
                     + pack(">i", 0)        # 136 -> Bitmap
                     + pack(">i", 144)      # 140 -> SmallInteger
                     # "arbitrary" objects from here on
                     # note that the following hashes/classids do not match up
                     # with a "real world" spur image, just like the classtable
                     + spur_hdr(0, 0, 0, 1) # 144 SmallInteger (class instance)
                     + pack(">q", 0)        # 152 body of SmallInteger
                     + spur_hdr(0, 1, 0, 2) # 160 SmallInteger class
                     + pack(">q", 0)        # 168 body of SmallInteger class
                     + spur_hdr(0, 2, 0, 3) # 176 Metaclass (class instance)
                     + pack(">q", 0)        # 184 body of Metaclass
                     + spur_hdr(0, 3, 0, 2) # 192 Metaclass class
                     + pack(">q", 0)        # 200 body of Metaclass class
                     + spur_hdr(0, 4, 0, 0) # 208 Array (class instance)
                     + pack(">q", 0)        # 216 body of Metaclass class
                     ) # bridge will be added later
    # second segment shall start at oop 1000 here
    second_segment = (spur_hdr(6, 4000, 2, 4)  # 1000 an Array
                     + pack(">i", 0)        # 1008 -> nil
                     + pack(">i", 16)       # 1012 -> false
                     + pack(">i", 32)       # 1016 -> true
                     + pack(">i", (42 << 1) | 1) # SmallInteger 42
                     + pack(">I", (ord(u'p') << 2) | 2) # Character p
                     + pack(">I", (ord(u'ü') << 2) | 2) # Character ü
                     + longs2str(0, 0))     # final bridge = stop
    first_segment = first_segment + \
            longs2str(1000 - len(first_segment) - 16, # bridge span
                      len(second_segment))            # next segment size
    body = first_segment + second_segment
    header_size = 16 * word_size
    image_1 = (SPUR_VERSION_HEADER       # 1
               + pack(">i", header_size) # 2 64 byte header
               + pack(">i", len(body))   # 3 body length
               + pack(">i", 0)           # 4 old base addresss unset
               + pack(">i", 112)         # 5 ptr to special objects array
               + "\x12\x34\x56\x78"      # 6 last hash
               + pack(">h", 480)         # 7 window 480 height
               +     pack(">h", 640)     #   window 640 width
               + pack(">i", 0)           # 8 not fullscreen
               + pack(">i", 0)           # 9 no extra memory
               + pack(">h", 0)           # 10 #stack pages
               + pack(">h", 0)           #    cog code size
               + pack(">i", 0)           # 11 eden bytes
               + pack(">h", 0)           # 12 max ext sem tab size (?)
               + pack(">h", 0)           #    unused
               + pack(">i", len(first_segment))  # 13 first segment size
               + pack(">i", 0)           # 14 free old space in image
               + ("\x00" * (header_size - (14 * word_size)))
               + body)
    r = imagereader_mock(image_1)
    # does not raise
    r.read_all()
    assert r.stream.pos == len(image_1)
    theArray = r.space.objtable["w_schedulerassociationpointer"]
    assert theArray.gethash() == 4000
    assert theArray.size() == 6
    assert theArray.fetch(r.space, 0).is_same_object(r.space.w_nil)
    assert theArray.fetch(r.space, 1).is_same_object(r.space.w_false)
    assert theArray.fetch(r.space, 2).is_same_object(r.space.w_true)
    assert isinstance(theArray.fetch(r.space, 3), model.W_SmallInteger)
    assert r.space.unwrap_int(theArray.fetch(r.space, 3)) == 42
    assert isinstance(theArray.fetch(r.space, 4), model.W_Character)
    assert r.space.unwrap_char(theArray.fetch(r.space, 4)) == 'p'
    assert isinstance(theArray.fetch(r.space, 5), model.W_Character)
    # we do not support unicode yet
    #assert r.space.unwrap_char(theArray.fetch(r.space, 5)) == u'ü'
