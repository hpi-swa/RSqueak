import py
import os

from rsqueakvm import constants
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.numeric import W_LargeIntegerWord
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject

from rpython.rtyper.lltypesystem import rffi
from rpython.rlib.rbigint import rbigint
from rpython.rlib.rarithmetic import r_uint

from .util import create_space, copy_to_module, cleanup_module, InterpreterForTest, read_image, external_call


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
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileDelete',
            stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filedelete_raises(monkeypatch):
    def remove(file_path):
        raise OSError()
    monkeypatch.setattr(os, "remove", remove)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myFile")]
            w_c = external_call(space,
                'FilePlugin',
                'primitiveFileDelete',
                stack)
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
        w_c = external_call(space,
            'FilePlugin',
            'primitiveDirectoryCreate',
            stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_dircreate_raises(monkeypatch):
    def mkdir(dir_path, mode):
        raise OSError()
    monkeypatch.setattr(os, "mkdir", mkdir)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myPrimDir")]
            w_c = external_call(space,
                'FilePlugin',
                'primitiveDirectoryCreate',
                stack)
    finally:
        monkeypatch.undo()


def test_fileplugin_dirdelete(monkeypatch):
    def rmdir(dir_path):
        assert dir_path == 'myPrimDir'
        return 0
    monkeypatch.setattr(os, "rmdir", rmdir)
    try:
        stack = [space.w(1), space.wrap_string("myPrimDir")]
        w_c = external_call(space,
            'FilePlugin',
            'primitiveDirectoryDelete',
            stack)
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
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            stack)
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
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            stack)
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
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_largeposint(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'dcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_LargeIntegerWord(space, space.w_LargePositiveInteger, r_uint(1633837924), 4)
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(4)]
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_filewrite_pointers(monkeypatch):
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            [1, 1, None, 1, 1])

def test_fileplugin_filewrite_bitmap(monkeypatch):
    def write(fd, data):
        assert len(data) == 4
        assert data == 'dcba'
        return 4
    monkeypatch.setattr(os, "write", write)

    content = W_DisplayBitmap(space, 1, 32)
    content._squeak_pixel_buffer[0] = rffi.r_uint(1633837924)
    try:
        stack = [space.w(1), space.w(1), content, space.w(1), space.w(1)]
        w_c = external_call(space,
            'FilePlugin',
            'primitiveFileWrite',
            stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_dirdelete_raises(monkeypatch):
    def rmdir(dir_path):
        raise OSError()
    monkeypatch.setattr(os, "rmdir", rmdir)

    try:
        with py.test.raises(PrimitiveFailedError):
            stack = [space.w(1), space.wrap_string("myPrimDir")]
            w_c = external_call(space,
                'FilePlugin',
                'primitiveDirectoryDelete',
                stack)
    finally:
        monkeypatch.undo()

def test_fileplugin_stdio_handles():
    handles = space.unwrap_array(external_call(space,
        'FilePlugin',
        'primitiveFileStdioHandles',
        [None]))
    assert handles[0].value == 0
    assert handles[1].value == 1
    assert handles[2].value == 2

def test_fileplugin_path_sep():
    import os
    sep = chr(external_call(space,
        'FilePlugin',
        'primitiveDirectoryDelimitor',
        [None]).value)
    assert sep == os.path.sep

def test_fileplugin_dir_lookup(monkeypatch):
    import os
    osstat = os.stat
    oslistdir = os.listdir
    def listdir(p):
        assert len(p) > 0
        if p == os.path.sep or p == "/rsqueaktest/":
            return ["foo", "bar", "OSError"]
        else:
            return oslistdir(p)
    realstat = os.stat(__file__)
    def stat(p):
        if p == "OSError": raise OSError
        if p in ["foo", "bar"]:
            return realstat
        return osstat(p)
    monkeypatch.setattr(os, "listdir", listdir)
    monkeypatch.setattr(os, "stat", stat)
    try:
        result = space.unwrap_array(
            external_call(space,
                'FilePlugin',
                'primitiveDirectoryLookup',
                [None, '', 1]))
        assert result[0].unwrap_string(space) == "foo"
        assert result[1].unwrap_long_untranslated(space) > realstat.st_ctime
        assert result[2].unwrap_long_untranslated(space) > realstat.st_mtime
        assert result[3] is space.w_false
        assert result[4].unwrap_long_untranslated(space) == realstat.st_size
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveDirectoryLookup',
                [None, '/rsqueaktest/', 4])
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveDirectoryLookup',
                [None, '/rsqueaktest/', 2])
    finally:
        monkeypatch.undo()

def test_fileplugin_file_open(monkeypatch):
    required_mode = -1
    osopen = os.open
    def open(path, mode, perm):
        if path not in ["new_file", __file__, os.path.dirname(__file__)]:
            return osopen(path, mode, perm)
        if path == os.path.dirname(__file__):
            raise OSError
        assert perm == 0666
        assert path in ["new_file", __file__]
        assert mode == (required_mode | os.O_BINARY)
        return 32
    monkeypatch.setattr(os, "open", open)
    try:
        required_mode = os.O_RDWR | os.O_CREAT
        assert external_call(space,
            'FilePlugin',
            'primitiveFileOpen',
            [None, "new_file", True]).value == 32
        required_mode = os.O_RDWR
        assert external_call(space,
            'FilePlugin',
            'primitiveFileOpen',
            [None, __file__, True]).value == 32
        required_mode = os.O_RDONLY
        assert external_call(space,
            'FilePlugin',
            'primitiveFileOpen',
            [None, __file__, False]).value == 32
        required_mode = os.O_RDONLY
        assert external_call(space,
            'FilePlugin',
            'primitiveFileOpen',
            [None, __file__, False]).value == 32
        assert external_call(space,
            'FilePlugin',
            'primitiveFileOpen',
            [None, "new_file", False]) is space.w_nil
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveDirectoryLookup',
                [None, os.path.dirname(__file__), True])
    finally:
        monkeypatch.undo()

def test_fileplugin_file_close(monkeypatch):
    def doclose(fd): return
    def dontclose(fd): raise OSError
    try:
        monkeypatch.setattr(os, "close", doclose)
        assert external_call(space,
            'FilePlugin',
            'primitiveFileClose',
            [None, 32]) is space.w_nil
        monkeypatch.setattr(os, "close", dontclose)
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileClose',
                [None, 32])
    finally:
        monkeypatch.undo()

def test_fileplugin_file_atend(monkeypatch):
    fd = os.open(__file__, os.O_RDONLY)
    try:
        assert external_call(space,
            'FilePlugin',
            'primitiveFileAtEnd',
            [None, fd]) is space.w_false
        os.lseek(fd, 0, os.SEEK_END)
        assert external_call(space,
            'FilePlugin',
            'primitiveFileAtEnd',
            [None, fd]) is space.w_true
    finally:
        os.close(fd)

def test_fileplugin_file_read(monkeypatch):
    with py.test.raises(PrimitiveFailedError):
        external_call(space, 'FilePlugin', 'primitiveFileRead', [None, 32, None, 1, 12])

    def raiseread(fd, count):
        raise OSError
    monkeypatch.setattr(os, "read", raiseread)
    try:
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileRead',
                [None, 32, "hello", 1, 5])
    finally:
        monkeypatch.undo()

    def read(fd, count):
        assert count == len("hello")
        return "hello"
    monkeypatch.setattr(os, "read", read)
    try:
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileRead',
                [None, 32, "123", 1, 5])
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileRead',
                [None, 32, "12345", 2, 5])

        w_out = space.w("123456")
        assert external_call(space,
            'FilePlugin',
            'primitiveFileRead',
            [None, 32, w_out, 1, 5]).value == 5
        assert w_out.unwrap_string(space) == "hello6"

        w_out = space.w("123456")
        assert external_call(space,
            'FilePlugin',
            'primitiveFileRead',
            [None, 32, w_out, 2, 5]).value == 5
        assert w_out.unwrap_string(space) == "1hello"
    finally:
        monkeypatch.undo()

def test_fileplugin_file_get_position(monkeypatch):
    fd = os.open(__file__, os.O_RDONLY)
    try:
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileGetPosition',
                [None, -1])
        assert external_call(space,
            'FilePlugin',
            'primitiveFileGetPosition',
            [None, fd]).value == 0
        os.lseek(fd, 32, os.SEEK_CUR)
        assert external_call(space,
            'FilePlugin',
            'primitiveFileGetPosition',
            [None, fd]).value == 32
    finally:
        os.close(fd)

def test_fileplugin_file_set_position(monkeypatch):
    fd = os.open(__file__, os.O_RDONLY)
    try:
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileSetPosition',
                [None, -1, 32])
        external_call(space,
            'FilePlugin',
            'primitiveFileSetPosition',
            [None, fd, 32])
        assert os.lseek(fd, 0, os.SEEK_CUR) == 32
    finally:
        os.close(fd)

def test_fileplugin_file_size(monkeypatch):
    fd = os.open(__file__, os.O_RDONLY)
    try:
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileSize',
                [None, -1])
        assert external_call(space,
            'FilePlugin',
            'primitiveFileSize',
            [None, fd]).value == os.fstat(fd).st_size
    finally:
        os.close(fd)

def test_fileplugin_file_truncate(monkeypatch):
    from rsqueakvm.plugins import file_plugin
    def truncate(fd, p):
        assert p == 12
        assert fd == 32
    def failtrunc(fd, p): raise OSError
    monkeypatch.setattr(file_plugin, "ftruncate", truncate)
    try:
        external_call(space,
            'FilePlugin',
            'primitiveFileTruncate',
            [None, 32, 12])
        monkeypatch.setattr(file_plugin, "ftruncate", failtrunc)
        with py.test.raises(PrimitiveFailedError):
            external_call(space,
                'FilePlugin',
                'primitiveFileTruncate',
                [None, 32, 12])
    finally:
        monkeypatch.undo()

def test_locale_plugin_primLang_fails(monkeypatch):
    from rpython.rlib import rlocale
    def setlocale(*args):
        return "C"
    monkeypatch.setattr(rlocale, "setlocale", setlocale)
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'LocalePlugin',
            'primitiveLanguage',
            [space.w_nil])

def test_locale_plugin_primLang(monkeypatch):
    from rpython.rlib import rlocale
    def setlocale(*args):
        return "en_US.UTF-8"
    monkeypatch.setattr(rlocale, "setlocale", setlocale)
    w_locale_str = external_call(space,
        'LocalePlugin',
        'primitiveLanguage',
        [space.w_nil])
    assert space.unwrap_string(w_locale_str) == "en"

def test_locale_plugin_primCountry_fails(monkeypatch):
    from rpython.rlib import rlocale
    def setlocale(*args):
        return "C"
    monkeypatch.setattr(rlocale, "setlocale", setlocale)
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'LocalePlugin',
            'primitiveCountry',
            [space.w_nil])

def test_locale_plugin_primCountry(monkeypatch):
    from rpython.rlib import rlocale
    def setlocale(*args):
        return "en_US.UTF-8"
    monkeypatch.setattr(rlocale, "setlocale", setlocale)
    w_locale_str = external_call(space,
        'LocalePlugin',
        'primitiveCountry',
        [space.w_nil])
    assert space.unwrap_string(w_locale_str) == "US"

def test_misc_primitiveIndexOfAsciiInString(monkeypatch):
    assert space.unwrap_int(
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("f"), space.w("foo"), space.w(1)])) == 1
    assert space.unwrap_int(
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("o"), space.w("foo"), space.w(1)])) == 2
    assert space.unwrap_int(
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("f"), space.w("foo"), space.w(2)])) == 0
    assert space.unwrap_int(
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("f"), space.w("foo"), space.w(100)])) == 0
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("f"), space.w("foo"), space.w(0)])
    with py.test.raises(PrimitiveFailedError):
        external_call(space,
            'MiscPrimitivePlugin',
            'primitiveIndexOfAsciiInString',
            [space.w_nil, space.wrap_char("f"), space.w("foo"), space.w(-1)])

def test_misc_primitiveStringHash(monkeypatch):
    space, interp, image, reader = read_image("Squeak4.3.image")
    prim_res = interp.perform(space.w("123"), "hash").unwrap_long_untranslated(space)
    from rsqueakvm.primitives.control import ExternalPlugins
    from rsqueakvm.plugins.misc_primitive_plugin import MiscPrimitivePlugin
    for p in ExternalPlugins:
        if p is MiscPrimitivePlugin:
            monkeypatch.delitem(p.primitives, "primitiveStringHash")
            break
    try:
        st_res = interp.perform(space.w("123"), "hash").unwrap_long_untranslated(space)
    finally:
        monkeypatch.undo()
    assert st_res == prim_res

def test_misc_primitiveCompareString(monkeypatch):
    space, interp, image, reader = read_image("Squeak4.3.image")
    prim_res = []
    st_res = []
    for x,y,f in [
            ("12", "1234", "="),
            ("1234", "12", "="),
            ("1234", "ab", "="),
            ("1234", "0b", "="),
            ("1234", "1234", "="),
            ("12", "1234", "compare:"),
            ("1234", "12", "compare:"),
            ("1234", "ab", "compare:"),
            ("1234", "0b", "compare:"),
            ("1234", "1234", "compare:")]:
        prim_res.append((x, y, f, interp.perform(space.w(x), f, w_arguments=[space.w(y)]) is space.w_true))

    from rsqueakvm.primitives.control import ExternalPlugins
    from rsqueakvm.plugins.misc_primitive_plugin import MiscPrimitivePlugin
    for p in ExternalPlugins:
        if p is MiscPrimitivePlugin:
            monkeypatch.delitem(p.primitives, "primitiveCompareString")
            break
    try:
        for x,y,f in [
                ("12", "1234", "="),
                ("1234", "12", "="),
                ("1234", "ab", "="),
                ("1234", "0b", "="),
                ("1234", "1234", "="),
                ("12", "1234", "compare:"),
                ("1234", "12", "compare:"),
                ("1234", "ab", "compare:"),
                ("1234", "0b", "compare:"),
                ("1234", "1234", "compare:")]:
            st_res.append((x, y, f, interp.perform(space.w(x), f, w_arguments=[space.w(y)]) is space.w_true))
    finally:
        monkeypatch.undo()
    assert st_res == prim_res


def test_float_array_at(monkeypatch):
    space, interp, image, reader = read_image("Squeak4.3.image")
    prim_res = []
    st_res = []


    w_FloatArray = space.smalltalk_at("FloatArray")
    w_ary = W_WordsObject(space, w_FloatArray, 1)

    from rpython.rlib.longlong2float import singlefloat2uint, uint2singlefloat
    from rpython.rlib.rarithmetic import r_singlefloat
    w_ary.setword(0, singlefloat2uint(r_singlefloat(1.5)))

    prim_res.append(interp.perform(w_ary, "at:", w_arguments=[space.w(1)]))
    prim_res.append(interp.perform(w_ary, "at:put:", w_arguments=[space.w(1), space.w(12.5)]))
    prim_res.append(interp.perform(w_ary, "at:", w_arguments=[space.w(1)]))
    assert [space.unwrap_float(w) for w in prim_res] == [1.5, 12.5, 12.5]

    from rsqueakvm.primitives.control import ExternalPlugins
    from rsqueakvm.plugins.float_array_plugin import FloatArrayPlugin

    for p in ExternalPlugins:
        if p is FloatArrayPlugin:
            monkeypatch.delitem(p.primitives, "primitiveAt")
            monkeypatch.delitem(p.primitives, "primitiveAtPut")
            break
    try:
        w_ary.setword(0, singlefloat2uint(r_singlefloat(1.5)))
        st_res.append(interp.perform(w_ary, "at:", w_arguments=[space.w(1)]))
        st_res.append(interp.perform(w_ary, "at:put:", w_arguments=[space.w(1), space.w(12.5)]))
        st_res.append(interp.perform(w_ary, "at:", w_arguments=[space.w(1)]))
        assert [space.unwrap_float(w) for w in st_res] == [1.5, 12.5, 12.5]
    finally:
        monkeypatch.undo()
