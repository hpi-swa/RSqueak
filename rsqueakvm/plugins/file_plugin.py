import os
import stat
import sys

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.numeric import W_Float, W_LargePositiveInteger1Word
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import index1_0
from rsqueakvm.util.system import IS_WINDOWS

from rpython.rlib import jit, rarithmetic


FilePlugin = Plugin()
os.stat_float_times(False)

try:
    std_fds = [sys.stdin.fileno(),
           sys.stdout.fileno(),
           sys.stderr.fileno()]
except ValueError:
    std_fds = [0, 1, 2]

if IS_WINDOWS:
    from rpython.rtyper.lltypesystem import rffi
    from rpython.rtyper.tool import rffi_platform as platform
    from rpython.translator.tool.cbuild import ExternalCompilationInfo

    eci = ExternalCompilationInfo(includes=["windows.h"])

    class CConfig:
        _compilation_info_ = eci
    config = platform.configure(CConfig)

    _chsize = rffi.llexternal("_chsize",
        [rffi.INT, rffi.LONG], rffi.INT,
        compilation_info=eci,
    )

    ftruncate = _chsize
else:
    # O_BINARY is only available on Windows
    ftruncate = os.ftruncate
    os.O_BINARY = 0

#should we implement primitiveDirectoryEntry ?
#should we implement primitiveHasFileAccess ?

@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveFileStdioHandles(interp, s_frame, w_rcvr):
    return interp.space.wrap_list(
        interp.space.wrap_int(0),
        interp.space.wrap_int(1),
        interp.space.wrap_int(2)
    )

@FilePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveFileDelete(interp, s_frame, w_rcvr, file_path):
    # we actually should ask the security plugin function sCDFfn for permissions
    try:
        os.remove(file_path)
    except OSError:
        raise PrimitiveFailedError
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveDirectoryDelimitor(interp, s_frame, w_rcvr):
    return interp.space.wrap_char(os.path.sep)

@FilePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveDirectoryCreate(interp, s_frame, w_rcvr, dir_path):
    # we actually should ask the security plugin function sCCPfn for permissions
    try:
        os.mkdir(dir_path, 0777)
    except OSError:
        raise PrimitiveFailedError
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveDirectoryDelete(interp, s_frame, w_rcvr, dir_path):
    # we actually should ask the security plugin function sCDPfn for permissions
    try:
        os.rmdir(dir_path)
    except OSError:
        raise PrimitiveFailedError
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object, str, index1_0])
def primitiveDirectoryLookup(interp, s_frame, w_file_directory, full_path, index):
    if full_path == '':
        contents = os.listdir(os.path.sep)
    else:
        if not os.path.isdir(full_path):
            raise PrimitiveFailedError
        contents = os.listdir(full_path)
    space = interp.space
    if index >= len(contents):
        return space.w_nil

    # should probably be sorted...
    contents
    py_name = contents[index]
    try:
        file_path = os.path.join(full_path, py_name)
    except OSError:
        raise PrimitiveFailedError
    try:
        file_info = os.stat(file_path)
    except OSError:
        raise PrimitiveFailedError

    w_name = space.wrap_string(py_name)
    w_creationTime = smalltalk_timestamp(space, file_info.st_ctime)
    w_modificationTime = smalltalk_timestamp(space, file_info.st_mtime)
    w_dirFlag = space.w_true if stat.S_IFDIR & file_info.st_mode else space.w_false
    w_fileSize = space.wrap_int(rarithmetic.intmask(file_info.st_size))

    return space.wrap_list([w_name, w_creationTime, w_modificationTime,
                            w_dirFlag, w_fileSize])

@FilePlugin.expose_primitive(unwrap_spec=[object, str, object])
def primitiveFileOpen(interp, s_frame, w_rcvr, file_path, w_writeable_flag):
    space = interp.space
    file_missing = not os.path.exists(file_path)
    if w_writeable_flag is space.w_true:
        if file_missing:
            mode = os.O_RDWR | os.O_CREAT
        else:
            mode = os.O_RDWR
    else:
        mode = os.O_RDONLY
        if file_missing:
            return space.w_nil
    mode |= os.O_BINARY
    try:
        file_descriptor = os.open(file_path, mode, 0666)
    except OSError:
        raise PrimitiveFailedError()
    return space.wrap_int(file_descriptor)

@FilePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveFileClose(interp, s_frame, w_rcvr, fd):
    try:
        os.close(fd)
    except OSError:
        raise PrimitiveFailedError()
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveFileAtEnd(interp, s_frame, w_rcvr, fd):
    file_info = os.fstat(fd)
    if os.lseek(fd, 0, os.SEEK_CUR) >= file_info.st_size:
        return interp.space.w_true
    else:
        return interp.space.w_false

@FilePlugin.expose_primitive(unwrap_spec=[object, int, object, index1_0, int])
def primitiveFileRead(interp, s_frame, w_rcvr, fd, target, start, count):
    if not isinstance(target, W_BytesObject):
        raise PrimitiveFailedError
    try:
        contents = os.read(fd, count)
    except OSError:
        raise PrimitiveFailedError
    space = interp.space
    len_read = len(contents)
    if target.size() < start + len_read:
        raise PrimitiveFailedError
    for i in range(len_read):
        target.setchar(start + i, contents[i])
    return space.wrap_int(len_read)

@FilePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveFileGetPosition(interp, s_frame, w_rcvr, fd):
    try:
        pos = os.lseek(fd, 0, os.SEEK_CUR)
    except OSError:
        raise PrimitiveFailedError
    else:
        return interp.space.wrap_positive_wordsize_int(rarithmetic.intmask(pos))

@FilePlugin.expose_primitive(unwrap_spec=[object, int, int])
def primitiveFileSetPosition(interp, s_frame, w_rcvr, fd, position):
    try:
        os.lseek(fd, position, os.SEEK_SET)
    except OSError:
        raise PrimitiveFailedError
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveFileSize(interp, s_frame, w_rcvr, fd):
    try:
        file_info = os.fstat(fd)
    except OSError:
        raise PrimitiveFailedError
    return interp.space.wrap_positive_wordsize_int(rarithmetic.intmask(file_info.st_size))

@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveFileStdioHandles(interp, s_frame, w_rcvr):
    # This primitive may give an error-code...
    # return an array with stdin, stdout, stderr
    space = interp.space
    return space.wrap_list([space.wrap_int(fd) for fd in std_fds])

@FilePlugin.expose_primitive(unwrap_spec=[object, int, object, index1_0, int])
def primitiveFileWrite(interp, s_frame, w_rcvr, fd, content, start, count):
    size = content.size()
    if isinstance(content, W_WordsObject):
        element_size = 4
    elif isinstance(content, W_Float):
        element_size = 8
    elif isinstance(content, W_DisplayBitmap):
        element_size = 4
    elif isinstance(content, W_BytesObject):
        element_size = 1
    elif isinstance(content, W_LargePositiveInteger1Word):
        element_size = 1
    else:
        raise PrimitiveFailedError

    string_content = interp.space.unwrap_string(content)
    byte_start = start * element_size
    byte_end = min(start + count, size) * element_size

    space = interp.space
    if not (byte_start >= 0 and byte_end > byte_start):
        return space.wrap_int(0)
    try:
        written = os.write(fd, string_content[byte_start:byte_end])
    except OSError:
        raise PrimitiveFailedError
    else:
        return space.wrap_positive_wordsize_int(rarithmetic.intmask(written / element_size))

@FilePlugin.expose_primitive(unwrap_spec=[object, int, int])
def primitiveFileTruncate(interp, s_frame, w_rcvr, fd, position):
    try:
        ftruncate(fd, position)
    except OSError:
        raise PrimitiveFailedError
    return w_rcvr

@FilePlugin.expose_primitive(unwrap_spec=[object, str, str, str])
def primitiveDirectorySetMacTypeAndCreator(interp, s_frame, w_rcvr, filename, type, creator):
    # TODO: this is a stub. "MacOS.SetCreatorAndType" is not available in my pypy build
    return w_rcvr

def smalltalk_timestamp(space, sec_since_epoch):
    import time
    from rsqueakvm.constants import SQUEAK_EPOCH_DELTA_MICROSECONDS
    from rpython.rlib.rarithmetic import r_uint
    secs_between_1901_and_1970 = SQUEAK_EPOCH_DELTA_MICROSECONDS / 1000000
    sec_since_1901 = r_uint(sec_since_epoch + secs_between_1901_and_1970)
    return space.wrap_uint(sec_since_1901)
