import py
import pytest

from rsqueakvm import storage_classes
from rsqueakvm.plugins.immutability.bytes import W_Immutable_BytesObject
from rsqueakvm.plugins.immutability.pointers import (
    select_immutable_pointers_class)
from rsqueakvm.plugins.immutability.words import W_Immutable_WordsObject
from rsqueakvm.model.pointers import W_PointersObject

from .util import create_space, cleanup_module


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
    assert w_ibytes.getchar(3) == "\x00"
    w_ibytes.setchar(3, "\xAA")
    assert w_ibytes.getchar(3) == "\x00"
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
