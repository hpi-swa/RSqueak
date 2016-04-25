import py
import os

from rsqueakvm import constants
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.numeric import W_LargePositiveInteger1Word
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.bytecodes import EXTERNAL_CALL

from rpython.rtyper.lltypesystem import lltype, rffi

from .util import create_space, copy_to_module, cleanup_module, TestInterpreter, very_slow_test

IMAGENAME = "anImage.image"

def mock(space, stack, context = None):
    mapped_stack = [space.w(x) for x in stack]
    frame = context
    for i in range(len(stack)):
        frame.as_context_get_shadow(space).push(stack[i])
    interp = TestInterpreter(space)
    interp.space.set_system_attribute(constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, IMAGENAME)
    return interp, frame, len(stack)

def _prim(space, code, stack, context = None):
    interp, w_frame, argument_count = mock(space, stack, context)
    prim_table[code](interp, w_frame.as_context_get_shadow(space), argument_count-1, context and context.as_context_get_shadow(space).w_method())
    res = w_frame.as_context_get_shadow(space).pop()
    s_frame = w_frame.as_context_get_shadow(space)
    assert not s_frame.stackdepth() - s_frame.tempsize()  # check args are consumed
    return res

def prim(code, stack, context = None):
    return _prim(space, code, stack, context)

def external_call(module_name, method_name, stack):
    w_description = W_PointersObject(space, space.classtable['w_Array'], 2)
    w_description.atput0(space, 0, space.w(module_name))
    w_description.atput0(space, 1, space.w(method_name))
    context = new_frame("<not called>", [w_description], stack[0], stack[1:])[0]
    return prim(EXTERNAL_CALL, stack, context)

def setup_module():
    space = create_space(bootstrap = True)
    wrap = space.w
    bootstrap_class = space.bootstrap_class
    new_frame = space.make_frame
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def test_fileplugin_filedelete(monkeypatch):
    def remove(file_path):
        assert file_path == 'myFile'
        return 0
    monkeypatch.setattr(os, "remove", remove)
    try:
        stack = [space.w(1), space.wrap_string("myFile")]
        w_c = external_call('FilePlugin', 'primitiveFileDelete', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filedelete_raises(monkeypatch):
    def remove(file_path):
        raise OSError()
    monkeypatch.setattr(os, "remove", remove)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myFile")]
            w_c = external_call('FilePlugin', 'primitiveFileDelete', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_dircreate(monkeypatch):
    def mkdir(dir_path, mode):
        assert dir_path == 'myPrimDir'
        assert mode == 0777
        return 0
    monkeypatch.setattr(os, "mkdir", mkdir)
    try:
        stack = [space.w(1), space.wrap_string("myPrimDir")]
        w_c = external_call('FilePlugin', 'primitiveDirectoryCreate', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_dircreate_raises(monkeypatch):
    def mkdir(dir_path, mode):
        raise OSError()
    monkeypatch.setattr(os, "mkdir", mkdir)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myPrimDir")]
            w_c = external_call('FilePlugin', 'primitiveDirectoryCreate', stack)
    finally:
        monkeypatch.undo()


def test_fileplugin_dirdelete(monkeypatch):
    def rmdir(dir_path):
        assert dir_path == 'myPrimDir'
        return 0
    monkeypatch.setattr(os, "rmdir", rmdir)
    try:
        stack = [space.w(1), space.wrap_string("myPrimDir")]
        w_c = external_call('FilePlugin', 'primitiveDirectoryDelete', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_bytes(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'abcd'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_BytesObject(space, space.w_String, 4)
    content.bytes = ["a", "b", "c", "d"]
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(4)]
        w_c = external_call('FilePlugin', 'primitiveFileWrite', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_words(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'dcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_WordsObject(space, space.w_String, 1)
    content.words = [rffi.r_uint(1633837924)]
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(1)]
        w_c = external_call('FilePlugin', 'primitiveFileWrite', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_float(monkeypatch):
    def write(fd, data):
        assert len(data) == 8
        assert data == 'hgfedcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = space.wrap_float(1.2926117907728089e+161)

    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(1)]
        w_c = external_call('FilePlugin', 'primitiveFileWrite', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_largeposint(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'dcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_LargePositiveInteger1Word(1633837924)
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(4)]
        w_c = external_call('FilePlugin', 'primitiveFileWrite', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_bitmap(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'dcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_DisplayBitmap(space, 1, 32)
    content._real_depth_buffer[0] = rffi.r_uint(1633837924)
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(1)]
        w_c = external_call('FilePlugin', 'primitiveFileWrite', stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_dirdelete_raises(monkeypatch):
    def rmdir(dir_path):
        raise OSError()
    monkeypatch.setattr(os, "rmdir", rmdir)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myPrimDir")]
            w_c = external_call('FilePlugin', 'primitiveDirectoryDelete', stack)
    finally:
        monkeypatch.undo()
