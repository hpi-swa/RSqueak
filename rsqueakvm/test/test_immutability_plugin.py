import py
import pytest

from rsqueakvm import storage_classes
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.immutability.bytes import W_Immutable_BytesObject
from rsqueakvm.plugins.immutability.pointers import (
    select_immutable_pointers_class)
from rsqueakvm.plugins.immutability.words import W_Immutable_WordsObject
from rsqueakvm.model.pointers import W_PointersObject

from .util import create_space, cleanup_module, external_call


def test_space():
    return create_space(bootstrap=True)

space = pytest.fixture(test_space)
bootstrap_class = None


def setup_module():
    global bootstrap_class, space
    v_space = test_space()
    space = v_space
    bootstrap_class = space.bootstrap_class


def teardown_module():
    cleanup_module(__name__)


def test_W_Immutable_BytesObject():
    w_class = bootstrap_class(0, format=storage_classes.BYTES)
    w_bytes = w_class.as_class_get_shadow(space).new(20)
    w_ibytes = W_Immutable_BytesObject(space, w_class, w_bytes.bytes)
    assert w_ibytes.is_immutable()
    assert w_ibytes.getclass(space).is_same_object(w_class)
    assert w_ibytes.size() == 20
    assert w_ibytes.getchar(3) == '\x00'
    w_ibytes.setchar(3, '\xAA')
    assert w_ibytes.getchar(3) == '\x00'
    py.test.raises(IndexError, lambda: w_ibytes.getchar(20))


def test_W_Immutable_PointersObjects():
    w_class = bootstrap_class(0)
    for i in range(20):
        w_pointers = W_PointersObject(space, w_class, i)
        cls = select_immutable_pointers_class(w_pointers.fetch_all(space))
        assert (i == len(cls._immutable_fields_) or
                cls._immutable_fields_ == ['_storage[*]'])
        placeholder = object()
        w_ipointers = cls(space, w_class, [placeholder] * i)
        assert w_ipointers.is_immutable()
        assert w_ipointers.getclass(space).is_same_object(w_class)
        assert w_ipointers.size() == i
        if i > 0:
            assert w_ipointers.fetch(space, 0) is placeholder
            w_ipointers.store(space, 0, space.w_true)
            assert w_ipointers.fetch(space, 0) is placeholder


def test_W_Immutable_WordsObject():
    w_class = bootstrap_class(0, format=storage_classes.WORDS)
    w_words = w_class.as_class_get_shadow(space).new(20)
    w_iwords = W_Immutable_WordsObject(space, w_class, w_words.words)
    assert w_iwords.is_immutable()
    assert w_iwords.getclass(space).is_same_object(w_class)
    assert w_iwords.size() == 20
    assert w_class.as_class_get_shadow(space).instsize() == 0
    assert w_iwords.getword(3) == 0
    w_iwords.setword(3, 42)
    assert w_iwords.getword(3) == 0
    py.test.raises(AssertionError, lambda: w_iwords.getword(20))

def test_primIsImmutable():
    w_class = bootstrap_class(0, format=storage_classes.WORDS)
    w_words = w_class.as_class_get_shadow(space).new(20)
    w_iwords = W_Immutable_WordsObject(space, w_class, w_words.words)
    assert external_call(space,
        'ImmutabilityPlugin',
        'primitiveIsImmutable',
        [w_words]) == space.w_false
    assert external_call(space,
        'ImmutabilityPlugin',
        'primitiveIsImmutable',
        [w_iwords]) == space.w_true

def test_primImmutableFrom_bytes():
    w_bytes_cls = bootstrap_class(0, format=storage_classes.BYTES)
    w_bytes_obj = w_bytes_cls.as_class_get_shadow(space).new(20)
    w_ibytes_obj = external_call(space,
        'ImmutabilityPlugin',
        'primitiveImmutableFrom',
        [w_bytes_cls, w_bytes_obj])
    assert w_ibytes_obj.is_immutable()
    assert w_ibytes_obj.getclass(space).is_same_object(w_bytes_cls)
    assert w_ibytes_obj.size() == 20
    assert w_ibytes_obj.getchar(3) == '\x00'
    w_ibytes_obj.setchar(3, '\xAA')
    assert w_ibytes_obj.getchar(3) == '\x00'

def test_primImmutableFrom_pointers():
    size = 10
    w_pointers_cls = bootstrap_class(0)
    w_pointers_obj = W_PointersObject(space, w_pointers_cls, size)
    w_pointers_obj.store(space, 0, space.w_true)
    w_ipointers_obj = external_call(space,
        'ImmutabilityPlugin',
        'primitiveImmutableFrom',
        [w_pointers_cls, w_pointers_obj])
    assert w_ipointers_obj.is_immutable()
    assert w_ipointers_obj.getclass(space).is_same_object(w_pointers_cls)
    assert w_ipointers_obj.size() == size
    assert w_ipointers_obj.fetch(space, 0) is space.w_true;
    w_ipointers_obj.store(space, 0, space.w_false)
    assert w_ipointers_obj.fetch(space, 0) is space.w_true;

def test_primImmutableFrom_words():
    w_words_cls = bootstrap_class(0, format=storage_classes.WORDS)
    w_words_obj = w_words_cls.as_class_get_shadow(space).new(20)
    w_iwords_obj = external_call(space,
        'ImmutabilityPlugin',
        'primitiveImmutableFrom',
        [w_words_cls, w_words_obj])
    assert w_iwords_obj.is_immutable()
    assert w_iwords_obj.getclass(space).is_same_object(w_words_cls)
    assert w_iwords_obj.size() == 20
    assert w_iwords_obj.getword(3) == 0
    w_iwords_obj.setword(3, 42)
    assert w_iwords_obj.getword(3) == 0

def test_primImmutableFrom_float():
    w_float_cls = bootstrap_class(0, format=storage_classes.FLOAT)
    w_float_obj = w_float_cls.as_class_get_shadow(space).new(20)
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'ImmutabilityPlugin',
            'primitiveImmutableFrom',
            [w_float_cls, w_float_obj])
