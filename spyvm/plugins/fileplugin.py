import os, stat
from spyvm import model, error
from spyvm.plugins.plugin import Plugin
from spyvm.primitives import PrimitiveFailedError, index1_0


FilePlugin = Plugin()
os.stat_float_times(False)

@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveDirectoryDelimitor(interp, s_frame, w_rcvr):
    return interp.space.wrap_char(os.path.sep)

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
    py_name = sorted(contents)[index]
    try:
        file_path = os.path.join(full_path, py_name)
    except OSError:
        raise PrimitiveFailedError
    file_info = os.stat(file_path)
    name = space.wrap_string(py_name)
    creationTime = smalltalk_timestamp(space, file_info.st_ctime)
    modificationTime = smalltalk_timestamp(space, file_info.st_mtime)
    dirFlag = space.w_true if stat.S_IFDIR & file_info.st_mode else space.w_false
    fileSize = space.wrap_int(file_info.st_size)
    return space.wrap_list([name, creationTime, modificationTime, dirFlag, fileSize])

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
    py_file = os.fdopen(fd)
    stat = os.fstat(fd)
    if py_file.tell() >= stat.st_size:
        return interp.space.w_true
    else:
        return interp.space.w_false

def smalltalk_timestamp(space, sec_since_epoch):
    import time
    from spyvm.primitives import secs_between_1901_and_1970
    sec_since_1901 = sec_since_epoch + secs_between_1901_and_1970
    return space.wrap_uint(sec_since_1901)
