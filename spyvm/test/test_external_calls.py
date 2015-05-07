import py, os, math, time
from spyvm import model, model_display, storage_contexts, constants, primitives, wrapper, display
from spyvm.primitives import prim_table, PrimitiveFailedError
from spyvm.plugins import bitblt
from rpython.rlib.rfloat import isinf, isnan
from rpython.rlib.rarithmetic import intmask, r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from .util import create_space, copy_to_module, cleanup_module, TestInterpreter, very_slow_test

IMAGENAME = "anImage.image"

def mock(space, stack, context = None):
    mapped_stack = [space.w(x) for x in stack]
    frame = context
    for i in range(len(stack)):
        frame.as_context_get_shadow(space).push(stack[i])
    interp = TestInterpreter(space)
    interp.space._image_name.set(IMAGENAME)
    return interp, frame, len(stack)

def _prim(space, code, stack, context = None):
    interp, w_frame, argument_count = mock(space, stack, context)
    prim_table[code](interp, w_frame.as_context_get_shadow(space), argument_count-1, context and context.as_context_get_shadow(space).w_method())
    res = w_frame.as_context_get_shadow(space).pop()
    s_frame = w_frame.as_context_get_shadow(space)
    assert not s_frame.stackdepth() - s_frame.tempsize() # check args are consumed
    return res

def prim(code, stack, context = None):
    return _prim(space, code, stack, context)

def external_call(module_name, method_name, stack):
    w_description = model.W_PointersObject(space, space.classtable['w_Array'], 2)
    w_description.atput0(space, 0, space.w(module_name))
    w_description.atput0(space, 1, space.w(method_name))
    context = new_frame("<not called>", [w_description], stack[0], stack[1:])[0]
    return prim(primitives.EXTERNAL_CALL, stack, context)

def setup_module():
    space = create_space(bootstrap = True)
    wrap = space.w
    bootstrap_class = space.bootstrap_class
    new_frame = space.make_frame
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

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
