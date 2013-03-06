import py
from spyvm import squeakimage
from spyvm.squeakimage import chrs2int, chrs2long, swapped_chrs2long
from spyvm import objspace

from struct import pack

space = objspace.ObjSpace()

# ----- helpers ----------------------------------------------

def ints2str(*ints):
    return pack(">" + "i" * len(ints), *ints)

def joinbits(values, lengths):
    result = 0
    for each, length in reversed(zip(values, lengths)):
        result = result << length
        result += each
    return result   

def imagestream_mock(string):
    import StringIO
    f = StringIO.StringIO(string)
    return squeakimage.Stream(f)

def imagereader_mock(string):
    stream = imagestream_mock(string)
    return squeakimage.reader_for_image(space, stream)


SIMPLE_VERSION_HEADER = pack(">i", 6502)
SIMPLE_VERSION_HEADER_LE = pack("<i", 6502)

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
    
def test_stream_swap():
    stream = imagestream_mock('\x66\x19\x00\x00')
    stream.swap = True
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
    py.test.raises(squeakimage.CorruptImageError, lambda: r.read_object())

def test_1wordobjectheader():
    s = ints2str(joinbits([3, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(space, 1, 2, 3, 4), 0 + l) == r.read_1wordobjectheader()

def test_1wordobjectheader2():
    s = ints2str(joinbits([3, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + (s * 3))
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(space, 1, 2, 3, 4), 0 + l) == r.read_1wordobjectheader()
    assert (squeakimage.ImageChunk(space, 1, 2, 3, 4), 4 + l) == r.read_1wordobjectheader()
    assert (squeakimage.ImageChunk(space, 1, 2, 3, 4), 8 + l) == r.read_1wordobjectheader()

def test_2wordobjectheader():
    s = ints2str(4200 + 1, joinbits([1, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(space, 1, 2, 4200, 4), 4 + l) == r.read_2wordobjectheader()

def test_3wordobjectheader():
    s = ints2str(1701 << 2, 4200 + 0, joinbits([0, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s)
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    assert (squeakimage.ImageChunk(space, 1701, 2, 4200, 4), 8 + l) == r.read_3wordobjectheader()
    
def test_read3wordheaderobject():
    size = 42
    s = ints2str(size << 2, 4200 + 0, joinbits([0, 1, 2, 3, 4], [2,6,4,5,12]))
    r = imagereader_mock(SIMPLE_VERSION_HEADER + s + SIMPLE_VERSION_HEADER * (size - 1))
    r.read_version()
    l = len(SIMPLE_VERSION_HEADER)
    chunk, pos = r.read_object()
    chunk0 = squeakimage.ImageChunk(space, size, 2, 4200, 4)
    chunk0.data = [6502] * (size - 1)
    assert pos == 8 + l
    assert chunk0 == chunk
    
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

def test_simple_image64():
    import sys
    if not sys.maxint == 2 ** 63 - 1:
      py.test.skip("on 32 bit platforms, we can't need to check for 64 bit images")
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
