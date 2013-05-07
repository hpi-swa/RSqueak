import py
import math
import socket
from spyvm import model, shadow
from spyvm.shadow import MethodNotFound
from spyvm import objspace, error, display
from rpython.rlib.rarithmetic import intmask, r_uint

mockclass = objspace.bootstrap_class

space = objspace.ObjSpace()
w_foo = space.wrap_string("foo")
w_bar = space.wrap_string("bar")

def joinbits(values, lengths):
    result = 0
    for each, length in reversed(zip(values, lengths)):
        result = result << length
        result += each
    return result


def test_new():
    w_mycls = mockclass(space, 0)
    w_myinstance = w_mycls.as_class_get_shadow(space).new()
    assert isinstance(w_myinstance, model.W_PointersObject)
    assert w_myinstance.getclass(space).is_same_object(w_mycls)
    assert w_myinstance.shadow_of_my_class(space) is w_mycls.as_class_get_shadow(space)

def test_new_namedvars():
    w_mycls = mockclass(space, 3)
    w_myinstance = w_mycls.as_class_get_shadow(space).new()
    assert isinstance(w_myinstance, model.W_PointersObject)
    assert w_myinstance.getclass(space).is_same_object(w_mycls)
    assert w_myinstance.fetch(space, 0) is space.w_nil
    py.test.raises(IndexError, lambda: w_myinstance.fetch(space, 3))
    w_myinstance.store(space, 1, w_myinstance)
    assert w_myinstance.fetch(space, 1) is w_myinstance

def test_bytes_object():
    w_class = mockclass(space, 0, format=shadow.BYTES)
    w_bytes = w_class.as_class_get_shadow(space).new(20)
    assert w_bytes.getclass(space).is_same_object(w_class)
    assert w_bytes.size() == 20
    assert w_class.as_class_get_shadow(space).instsize() == 0
    assert w_bytes.getchar(3) == "\x00"
    w_bytes.setchar(3, "\xAA")
    assert w_bytes.getchar(3) == "\xAA"
    assert w_bytes.getchar(0) == "\x00"
    py.test.raises(IndexError, lambda: w_bytes.getchar(20))

def test_word_object():
    w_class = mockclass(space, 0, format=shadow.WORDS)
    w_bytes = w_class.as_class_get_shadow(space).new(20)
    assert w_bytes.getclass(space).is_same_object(w_class)
    assert w_bytes.size() == 20
    assert w_class.as_class_get_shadow(space).instsize() == 0
    assert w_bytes.getword(3) == 0
    w_bytes.setword(3, 42)
    assert w_bytes.getword(3) == 42
    assert w_bytes.getword(0) == 0
    py.test.raises(IndexError, lambda: w_bytes.getword(20))

def test_method_lookup():
    class mockmethod(object):
        def __init__(self, val):
            self.val = val
        def as_compiledmethod_get_shadow(self, space):
            return self.val
    w_class = mockclass(space, mockmethod(0))
    shadow = w_class.as_class_get_shadow(space)
    shadow.installmethod(w_foo, mockmethod(1))
    shadow.installmethod(w_bar, mockmethod(2))
    w_subclass = mockclass(space, 0, w_superclass=w_class)
    subshadow = w_subclass.as_class_get_shadow(space)
    assert subshadow.s_superclass() is shadow
    subshadow.installmethod(w_foo, mockmethod(3))
    shadow.initialize_methoddict()
    subshadow.initialize_methoddict()
    assert shadow.lookup(w_foo) == 1
    assert shadow.lookup(w_bar) == 2
    py.test.raises(MethodNotFound, shadow.lookup, "zork")
    assert subshadow.lookup(w_foo) == 3
    assert subshadow.lookup(w_bar) == 2
    py.test.raises(MethodNotFound, subshadow.lookup, "zork")

def test_w_compiledin():
    w_super = mockclass(space, 0)
    w_class = mockclass(space, 0, w_superclass=w_super)
    supershadow = w_super.as_class_get_shadow(space)
    supershadow.installmethod(w_foo, model.W_CompiledMethod(0))
    classshadow = w_class.as_class_get_shadow(space)
    classshadow.initialize_methoddict()
    assert classshadow.lookup(w_foo).w_compiledin is w_super

def test_compiledmethod_setchar():
    w_method = model.W_CompiledMethod(3)
    w_method.setchar(0, "c")
    assert w_method.bytes == list("c\x00\x00")

def test_hashes():
    w_five = model.W_SmallInteger(5)
    assert w_five.gethash() == 5
    w_class = mockclass(space, 0)
    w_inst = w_class.as_class_get_shadow(space).new()
    assert w_inst.hash == w_inst.UNASSIGNED_HASH
    h1 = w_inst.gethash()
    h2 = w_inst.gethash()
    assert h1 == h2
    assert h1 == w_inst.hash

def test_compiledmethod_at0():
    w_method = model.W_CompiledMethod()
    w_method.bytes = list("abc")
    w_method.header = 100
    w_method.setliterals(['lit1', 'lit2'])
    w_method.literalsize = 2
    assert space.unwrap_int(w_method.at0(space, 0)) == 100
    assert w_method.at0(space, 4) == 'lit1'
    assert w_method.at0(space, 8) == 'lit2'
    assert space.unwrap_int(w_method.at0(space, 12)) == ord('a')
    assert space.unwrap_int(w_method.at0(space, 13)) == ord('b')
    assert space.unwrap_int(w_method.at0(space, 14)) == ord('c')

def test_compiledmethod_atput0():
    w_method = model.W_CompiledMethod(3)
    newheader = joinbits([0,2,0,0,0,0],[9,8,1,6,4,1])
    assert w_method.getliteralsize() == 0
    w_method.atput0(space, 0, space.wrap_int(newheader))
    assert w_method.getliteralsize() == 8 # 2 from new header * BYTES_PER_WORD (= 4)
    w_method.atput0(space, 4, 'lit1')
    w_method.atput0(space, 8, 'lit2')
    w_method.atput0(space, 12, space.wrap_int(ord('a')))
    w_method.atput0(space, 13, space.wrap_int(ord('b')))
    w_method.atput0(space, 14, space.wrap_int(ord('c')))
    assert space.unwrap_int(w_method.at0(space, 0)) == newheader
    assert w_method.at0(space, 4) == 'lit1'
    assert w_method.at0(space, 8) == 'lit2'
    assert space.unwrap_int(w_method.at0(space, 12)) == ord('a')
    assert space.unwrap_int(w_method.at0(space, 13)) == ord('b')
    assert space.unwrap_int(w_method.at0(space, 14)) == ord('c')

def test_compiledmethod_atput0_not_aligned():
    header = joinbits([0,2,0,0,0,0],[9,8,1,6,4,1])
    w_method = model.W_CompiledMethod(3, header)
    with py.test.raises(error.PrimitiveFailedError):
        w_method.atput0(space, 7, 'lit1')
    with py.test.raises(error.PrimitiveFailedError):
        w_method.atput0(space, 9, space.wrap_int(5))

def test_is_same_object(w_o1=model.W_PointersObject(space, None,0), w_o2=None):
    if w_o2 is None:
        w_o2 = w_o1
    assert w_o1.is_same_object(w_o2)
    assert w_o2.is_same_object(w_o1)

def test_not_is_same_object(w_o1=model.W_PointersObject(space, None,0),w_o2=model.W_PointersObject(space, None,0)):
    assert not w_o1.is_same_object(w_o2)
    assert not w_o2.is_same_object(w_o1)
    w_o2 = model.W_SmallInteger(2)
    assert not w_o1.is_same_object(w_o2)
    assert not w_o2.is_same_object(w_o1)
    w_o2 = model.W_Float(5.5)
    assert not w_o1.is_same_object(w_o2)
    assert not w_o2.is_same_object(w_o1)

def test_intfloat_is_same_object():
    test_is_same_object(model.W_SmallInteger(1), model.W_SmallInteger(1))
    test_is_same_object(model.W_SmallInteger(100), model.W_SmallInteger(100))
    test_is_same_object(model.W_Float(1.100), model.W_Float(1.100))

def test_intfloat_notis_same_object():
    test_not_is_same_object(model.W_SmallInteger(1), model.W_Float(1))
    test_not_is_same_object(model.W_Float(100), model.W_SmallInteger(100))
    test_not_is_same_object(model.W_Float(1.100), model.W_Float(1.200))
    test_not_is_same_object(model.W_SmallInteger(101), model.W_SmallInteger(100))

def test_charis_same_object():
    test_is_same_object(space.wrap_char('a'), space.wrap_char('a'))
    test_is_same_object(space.wrap_char('d'), space.wrap_char('d'))

def test_not_charis_same_object():
    test_not_is_same_object(space.wrap_char('a'), space.wrap_char('d'))
    test_not_is_same_object(space.wrap_char('d'), space.wrap_int(3))
    test_not_is_same_object(space.wrap_char('d'), space.wrap_float(3.0))

def test_become_pointers():
    w_clsa = mockclass(space, 3)
    w_a = w_clsa.as_class_get_shadow(space).new()

    w_clsb = mockclass(space, 4)
    w_b = w_clsb.as_class_get_shadow(space).new()

    hasha = w_a.gethash()
    hashb = w_b.gethash()

    w_a.store(space, 0, w_b)
    w_b.store(space, 1, w_a)

    res = w_a.become(w_b)
    assert res
    assert w_a.gethash() == hashb
    assert w_b.gethash() == hasha

    assert w_a.getclass(space).is_same_object(w_clsb)
    assert w_b.getclass(space).is_same_object(w_clsa)

    assert w_b.fetch(space, 0) is w_b
    assert w_a.fetch(space, 1) is w_a

def test_become_with_shadow():
    w_clsa = mockclass(space, 3)
    s_clsa = w_clsa.as_class_get_shadow(space)
    w_clsb = mockclass(space, 4)
    s_clsb = w_clsb.as_class_get_shadow(space)
    res = w_clsa.become(w_clsb)
    assert res
    assert w_clsa.as_class_get_shadow(space) is s_clsb
    assert s_clsa._w_self is w_clsb
    assert w_clsb.as_class_get_shadow(space) is s_clsa
    assert s_clsb._w_self is w_clsa

def test_word_atput():
    i = model.W_SmallInteger(100)
    b = model.W_WordsObject(space, None, 1)
    b.atput0(space, 0, i)
    assert 100 == b.getword(0)
    i = space.classtable['w_LargePositiveInteger'].as_class_get_shadow(space).new(4)
    i.atput0(space, 3, space.wrap_int(192))
    b.atput0(space, 0, i)
    assert b.getword(0) == 3221225472

def test_word_at():
    b = model.W_WordsObject(space, None, 1)
    b.setword(0, 100)
    r = b.at0(space, 0)
    assert isinstance(r, model.W_SmallInteger)
    assert space.unwrap_int(r) == 100

    b.setword(0, 3221225472)
    r = b.at0(space, 0)
    assert isinstance(r, (model.W_BytesObject, model.W_LargePositiveInteger1Word))
    assert r.size() == 4

def test_float_at():
    b = model.W_Float(64.0)
    r = b.fetch(space, 0)
    assert isinstance(r, model.W_LargePositiveInteger1Word)
    assert r.size() == 4
    assert space.unwrap_int(r.at0(space, 0)) == 0
    assert space.unwrap_int(r.at0(space, 1)) == 0
    assert space.unwrap_int(r.at0(space, 2)) == 80
    assert space.unwrap_int(r.at0(space, 3)) == 64
    r = b.fetch(space, 1)
    assert isinstance(r, model.W_SmallInteger)
    assert r.value == 0

def test_float_at_put():
    target = model.W_Float(1.0)
    for f in [1.0, -1.0, 1.1, 64.4, -0.0, float('nan'), float('inf')]:
        source = model.W_Float(f)
        target.store(space, 0, source.fetch(space, 0))
        target.store(space, 1, source.fetch(space, 1))
        if math.isnan(f):
            assert math.isnan(target.value)
        else:
            assert target.value == f

def test_float_hash():
    target = model.W_Float(1.1)
    assert target.gethash() == model.W_Float(1.1).gethash()
    target.store(space, 0, space.wrap_int(42))
    assert target.gethash() != model.W_Float(1.1).gethash()

def test_large_positive_integer_1word_at():
    b = model.W_LargePositiveInteger1Word(-1)
    for i in range(4):
        r = b.at0(space, i)
        assert isinstance(r, model.W_SmallInteger)
        assert space.unwrap_int(r) == 0xff
    assert b.value == -1

def test_large_positive_integer_1word_at_put():
    target = model.W_LargePositiveInteger1Word(0)
    source = model.W_LargePositiveInteger1Word(-1)
    for i in range(4):
        target.atput0(space, i, source.at0(space, i))
        assert target.at0(space, i) == source.at0(space, i)
    assert hex(r_uint(target.value)) == hex(r_uint(source.value))

@py.test.mark.skipif("socket.gethostname() == 'precise32'")
def test_display_bitmap():
    # XXX: Patch SDLDisplay -> get_pixelbuffer() to circumvent
    # double-free bug
    def get_pixelbuffer(self):
        from rpython.rtyper.lltypesystem import lltype, rffi
        return lltype.malloc(rffi.ULONGP.TO, self.width * self.height * 32, flavor='raw')
    display.SDLDisplay.get_pixelbuffer = get_pixelbuffer
    d = display.SDLDisplay("test")
    d.set_video_mode(10, 10, 1)

    target = model.W_DisplayBitmap.create(space, space.w_Array, 100, 1, d)
    target.setword(0, 0xFF00)
    assert bin(target.getword(0)) == bin(0xFF00)
    target.setword(0, 0x00FF00FF)
    assert bin(target.getword(0)) == bin(0x00FF00FF)
    target.setword(0, 0xFF00FF00)
    assert bin(target.getword(0)) == bin(0xFF00FF00)
    for i in xrange(8):
        assert target.pixelbuffer[i] == 0xff000000
    for i in xrange(8, 16):
        assert target.pixelbuffer[i] == 0xffffffff
    for i in xrange(16, 24):
        assert target.pixelbuffer[i] == 0xff000000
    for i in xrange(24, 32):
        assert target.pixelbuffer[i] == 0xffffffff


@py.test.mark.skipif("socket.gethostname() == 'precise32'")
def test_weak_pointers():
    from spyvm.shadow import WEAK_POINTERS

    w_cls = mockclass(space, 1)
    s_cls = w_cls.as_class_get_shadow(space)
    s_cls.instance_kind = WEAK_POINTERS

    weak_object = s_cls.new()
    referenced = model.W_SmallInteger(10)
    weak_object.store(space, 0, referenced)

    assert weak_object.fetch(space, 0) is referenced
    del referenced
    assert weak_object.fetch(space, 0) is space.w_nil
