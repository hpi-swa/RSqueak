import platform

from rsqueakvm.plugins.database import dbm
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives.bytecodes import *
from rsqueakvm.plugins.database.model import W_DBObject, BLOB


def _import_sqpyte():
    try:
        assert "64bit" in platform.architecture()[0]
        from sqpyte import interpreter
        from sqpyte.capi import CConfig
        return interpreter, CConfig
    except (ImportError, AssertionError):
        return None, None
interpreter, CConfig = _import_sqpyte()


def _patch_class_shadow():
    from rsqueakvm.storage_classes import ClassShadow
    from rsqueakvm.util.version import elidable_for_version
    old_make_pointers = ClassShadow.make_pointers_object

    def db_make_pointers_object(self, w_cls, size):
        if dbm.driver is not None and self.inherits_from_dbobject():
            return W_DBObject(self.space, w_cls, size)
        else:
            return old_make_pointers(self, w_cls, size)

    def inherits_from_dbobject(self):
        if self.getname() == "DBObject":
            return True
        s_superclass = self.s_superclass()
        if s_superclass:
            return s_superclass.inherits_from_dbobject()
        return False
    ClassShadow.make_pointers_object = db_make_pointers_object
    ClassShadow.inherits_from_dbobject = elidable_for_version(0)(
        inherits_from_dbobject)
_patch_class_shadow()


DatabasePlugin = Plugin()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, str, bool])
def primitiveSQLConnect(interp, s_frame, w_rcvr, filename, sqpyte):
    if interpreter is None:
        raise PrimitiveFailedError('sqpyte not found')
    if sqpyte:
        db_handle = dbm.connect(interpreter.SQPyteDB, filename)
    else:
        db_handle = dbm.connect(interpreter.SQLite3DB, filename)
    return interp.space.wrap_int(db_handle)


@DatabasePlugin.expose_primitive(clean_stack=False)
def primitiveSQLExecute(interp, s_frame, argcount):
    if not 2 <= argcount <= 3:
        raise PrimitiveFailedError(
            'wrong number of arguments: %s' % argcount)
    space = interp.space
    args = None
    if argcount == 3:
        args = interp.space.unwrap_array(s_frame.pop())

    arg3_w = s_frame.pop()
    sql = space.unwrap_string(arg3_w)
    arg2_w = s_frame.pop()
    db_handle = space.unwrap_longlong(arg2_w)
    connection = dbm.get_connection(db_handle)
    cursor_handle = dbm.execute(interp.space, connection, sql, args)

    return space.wrap_int(cursor_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLNext(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).next(interp.space)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnCount(interp, s_frame, w_rcvr, cursor_handle):
    return interp.space.wrap_int(dbm.cursor(cursor_handle).column_count)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnNames(interp, s_frame, w_rcvr, cursor_handle):
    return interp.space.wrap_list([
        interp.space.wrap_string(c) for c in
        dbm.cursor(cursor_handle).column_names])


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int, int])
def primitiveSQLColumnName(interp, s_frame, w_rcvr, cursor_handle, index):
    if index < 1:
        raise PrimitiveFailedError('Index must be >= 1')

    # Smalltalk counts from 1, rest of world from 0
    return interp.space.wrap_string(
        dbm.cursor(cursor_handle).column_names[index - 1])


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLClose(interp, s_frame, w_rcvr, db_handle):
    return interp.space.wrap_bool(dbm.close(db_handle))


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLModeSwitch(interp, s_frame, w_rcvr, mode):
    if interpreter is None:
        raise PrimitiveFailedError('sqpyte not found')
    if mode == 1:
        dbm.driver = interpreter.SQLite3DB
    elif mode == 2:
        dbm.driver = interpreter.SQPyteDB
    else:
        dbm.driver = None
    return interp.space.w_nil


@DatabasePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveSetDBFile(interp, s_frame, w_rcvr, db_file_name):
    dbm.db_file_name = db_file_name
    return interp.space.w_nil


@DatabasePlugin.expose_primitive(unwrap_spec=[object])
def primitiveCloseDBObject(interp, s_frame, w_rcvr):
    return interp.space.wrap_bool(dbm.connection().close())


@DatabasePlugin.expose_primitive(unwrap_spec=[object])
def primitiveSQLAllInstances(interp, s_frame, w_class):
    class_name = w_class.classname(interp.space).split(' ')[0]
    handle = dbm.connection()
    cursor_handle = dbm.execute(interp.space, handle,
                                'SELECT * FROM %s;' % class_name)
    return interp.space.wrap_int(cursor_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLNextObject(interp, s_frame, w_rcvr, cursor_handle):
    if CConfig is None:
        raise PrimitiveFailedError('sqpyte not found')
    space = interp.space
    cursor = dbm.cursor(cursor_handle)
    row = cursor.raw_next(space)
    if not row:
        return space.w_nil
    w_id = None
    num_cols = len(row)
    cache = [None] * num_cols
    for i in range(num_cols):
        name = cursor.column_names[i]
        if name == 'id':
            w_id = row[i]
        else:
            n0 = int(name[9:])  # strip 'inst_var_'
            class_name = w_rcvr.classname(interp.space).split(' ')[0]
            if W_DBObject.state.get_column_type(class_name, n0) is BLOB:
                db_id = space.unwrap_int(row[i])
                cache[n0] = W_DBObject.state.db_objects[db_id]
            else:
                cache[n0] = row[i]
    if w_id is None:
        raise PrimitiveFailedError('Could not find w_id')
    return W_DBObject(space, w_rcvr, num_cols, w_id=w_id, cache=cache)
