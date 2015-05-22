# -*- coding: utf-8 -*-
import py, StringIO, sys
from struct import pack
from spyvm import squeakimage, error
from spyvm.util.stream import chrs2int, chrs2long, swapped_chrs2long
from .util import create_space, copy_to_module, cleanup_module

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

def test_read_normal_spur_header():
    # Array of pointers
    n_slots = 42
    objbytes = ints2str(joinbits([48, 0, n_slots], [22, 2, 8]),
            joinbits([10, 0, 2, 0], [22, 2, 5, 3])) + ints2str(0) * n_slots
    r = imagereader_mock(SPUR_VERSION_HEADER + objbytes)
    r.read_version()
    actualChunk, pos = r.readerStrategy.read_object()
    expectedChunk = squeakimage.ImageChunk(size=n_slots, format=2, classid=10,
            hash=48, data=[0] * n_slots)
    assert expectedChunk == actualChunk
    assert pos == len(SPUR_VERSION_HEADER)

def test_read_long_spur_header():
    n_slots = 3000
    objbytes = longs2str(joinbits([n_slots, 255], [56, 8])) + ints2str(
            joinbits([55, 0, 255], [22, 2, 8]),
            joinbits([10, 0, 2, 0], [22, 2, 5, 3])) + ints2str(0) * n_slots
    r = imagereader_mock(SPUR_VERSION_HEADER + objbytes)
    r.read_version()
    actualChunk, pos = r.readerStrategy.read_object()
    expectedChunk = squeakimage.ImageChunk(size=n_slots, format=2, classid=10,
            hash=55, data=[0] * n_slots)
    assert expectedChunk == actualChunk
    assert pos == len(SPUR_VERSION_HEADER) + 8
    
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

def spur_hdr(n_slots, hash, format, classid):
    return ints2str(joinbits([hash, 0, n_slots], [22, 2, 8]),
                    joinbits([classid, 0, format], [22, 2, 5, 3]))

def test_simple_spur_image():
    word_size = 4
    # first segment
    first_segment = (spur_hdr(0, 0, 0, 0)   #   0 nil
                     + spur_hdr(0, 0, 0, 0) #   8 false
                     + spur_hdr(0, 0, 0, 0) #  16 true
                     + spur_hdr(0, 0, 0, 0) #  24 freeList
                     + spur_hdr(1, 0, 4, 0) #  32 hiddenRoots
                     + pack(">i", 44)       #  40 ptr to 1st class table page
                     + spur_hdr(4, 0, 4, 0) #  44 1st class table page
                     + pack(">i", 100)      #  52 ptr to first class (here SmallInteger)
                     + pack(">i", 108)      #  56 ptr to SmallInteger class
                     + pack(">i", 116)      #  60 ptr to Metaclass
                     + pack(">i", 124)      #  64 ptr to Metaclass class
                     + spur_hdr(6, 0, 4, 0) #  68 special objects array
                     + pack(">i", 0)        #  76 ptr to nil
                     + pack(">i", 8)        #  80 ... false
                     + pack(">i", 16)       #  84 ... true
                     + pack(">i", 0)        #  88 ... schedulerassocptr
                     + pack(">i", 0)        #  92 ... Bitmap
                     + pack(">i", 100)      #  96 ... SmallInteger
                     # "arbitrary" objects from here on
                     + spur_hdr(0, 0, 0, 1) # 100 SmallInteger (class instance)
                     + spur_hdr(0, 1, 0, 2) # 108 SmallInteger class
                     + spur_hdr(0, 2, 0, 3) # 116 Metaclass (class instance)
                     + spur_hdr(0, 3, 0, 2) # 124 Metaclass class
                     + ints2str(0, 0))      # 128 final bridge = stop at 136
    body = first_segment
    header_size = 16 * word_size
    image_1 = (SPUR_VERSION_HEADER       # 1
               + pack(">i", header_size) # 2 64 byte header
               + pack(">i", len(body))   # 3 body length
               + pack(">i", 0)           # 4 old base addresss unset
               + pack(">i", 68)          # 5 ptr to special objects array
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

def test_simple_spur_image_with_segments():
    word_size = 4
    # first segment
    # use 3000 + x as hash for debugging purposes (easier to identify g_objects)
    first_segment = (spur_hdr(0, 3000, 0, 0)   #   0 nil
                     + spur_hdr(0, 3008, 0, 0) #   8 false
                     + spur_hdr(0, 3016, 0, 0) #  16 true
                     + spur_hdr(0, 3024, 0, 0) #  24 freeList
                     + spur_hdr(1, 3032, 4, 0) #  32 hiddenRoots
                     + pack(">i", 44)       #  40 ptr to 1st class table page
                     + spur_hdr(5, 3044, 4, 0) #  44 1st class table page
                     + pack(">i", 104)      #  52 ptr to first class (here SmallInteger)
                     + pack(">i", 112)      #  56 ptr to SmallInteger class
                     + pack(">i", 120)      #  60 ptr to Metaclass
                     + pack(">i", 128)      #  64 ptr to Metaclass class
                     + pack(">i", 136)      #  68 ptr to Array
                     + spur_hdr(6, 3072, 4, 0) #  72 special objects array
                     + pack(">i", 0)        #  80 ptr to nil
                     + pack(">i", 8)        #  84 ... false
                     + pack(">i", 16)       #  84 ... true
                     + pack(">i", 1000)     #  92 ... alienated scheduler association for this test
                     + pack(">i", 0)        #  96 ... Bitmap
                     + pack(">i", 104)      # 100 ... SmallInteger
                     # "arbitrary" objects from here on
                     + spur_hdr(0, 0, 0, 1) # 104 SmallInteger (class instance)
                     + spur_hdr(0, 1, 0, 2) # 112 SmallInteger class
                     + spur_hdr(0, 2, 0, 3) # 120 Metaclass (class instance)
                     + spur_hdr(0, 3, 0, 2) # 128 Metaclass class
                     + spur_hdr(0, 4, 0, 0))# 136 Array (class instance)
                     # bridge will be added later
    # second segment shall start at oop 1000 here
    second_segment = (spur_hdr(6, 4000, 2, 4)  # 1000 an Array
                     + pack(">i", 0)        # 1008 -> nil
                     + pack(">i", 8)        # 1012 -> false
                     + pack(">i", 16)       # 1016 -> true
                     + pack(">i", (42 << 1) | 1) # SmallInteger 42
                     + pack(">I", (ord(u'p') << 2) | 2) # Character p
                     + pack(">I", (ord(u'ü') << 2) | 2) # Character ü
                     + ints2str(0, 0))      # final bridge = stop
    first_segment = first_segment + \
            ints2str(1000 - len(first_segment) - 8, # bridge span
                     len(second_segment))           # next segment size
    body = first_segment + second_segment
    header_size = 16 * word_size
    image_1 = (SPUR_VERSION_HEADER       # 1
               + pack(">i", header_size) # 2 64 byte header
               + pack(">i", len(body))   # 3 body length
               + pack(">i", 0)           # 4 old base addresss unset
               + pack(">i", 72)          # 5 ptr to special objects array
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
    from spyvm import model
    assert isinstance(theArray.fetch(r.space, 3), model.W_SmallInteger)
    assert r.space.unwrap_int(theArray.fetch(r.space, 3)) == 42
    assert isinstance(theArray.fetch(r.space, 4), model.W_Character)
    assert r.space.unwrap_char(theArray.fetch(r.space, 4)) == 'p'
    assert isinstance(theArray.fetch(r.space, 5), model.W_Character)
    # we do not support unicode yet
    #assert r.space.unwrap_char(theArray.fetch(r.space, 5)) == u'ü'
