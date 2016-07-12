# -*- coding: utf-8 -*-

from rsqueakvm.database import dbm
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives.bytecodes import *
from rsqueakvm.model.database import W_DBObject

from sqpyte import interpreter
from sqpyte.capi import CConfig

DatabasePlugin = Plugin()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, str, bool])
def primitiveSQLConnect(interp, s_frame, w_rcvr, filename, sqpyte):
    if sqpyte:
        return dbm.connect(interp.space, interpreter.SQPyteDB, filename)
    return dbm.connect(interp.space, interpreter.SQLite3DB, filename)


@DatabasePlugin.expose_primitive(clean_stack=False)
def primitiveSQLExecute(interp, s_frame, argcount):
    if not 2 <= argcount <= 3:
        raise PrimitiveFailedError(
            'wrong number of arguments: %s' % argcount)

    args = None
    if argcount == 3:
        args = interp.space.unwrap_array(s_frame.pop())

    arg3_w = s_frame.pop()
    sql = interp.space.unwrap_string(arg3_w)
    arg2_w = s_frame.pop()
    db_handle = interp.space.unwrap_longlong(arg2_w)
    connection = dbm.get_connection(db_handle)
    cursor_handle = dbm.execute(connection, sql, args)

    return interp.space.wrap_int(cursor_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLNext(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).next()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnCount(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).column_count()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnNames(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).column_names()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int, int])
def primitiveSQLColumnName(interp, s_frame, w_rcvr, cursor_handle, index):
    if index < 1:
        raise PrimitiveFailedError('Index must be >= 1')

    # Smalltalk counts from 1, rest of world from 0
    return dbm.cursor(cursor_handle).column_name(index - 1)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLClose(interp, s_frame, w_rcvr, db_handle):
    return dbm.close(interp.space, db_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLModeSwitch(interp, s_frame, w_rcvr, mode):
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
    dbm.connection(interp.space).close()
    return interp.space.w_nil


@DatabasePlugin.expose_primitive(unwrap_spec=[object])
def primitiveSQLAllInstances(interp, s_frame, w_class):
    class_name = w_class.classname(interp.space).split(' ')[0]
    handle = dbm.connection(interp.space)
    cursor_handle = dbm.execute(handle, 'SELECT * FROM %s;' % class_name)
    return interp.space.wrap_int(cursor_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLNextObject(interp, s_frame, w_rcvr, cursor_handle):
    query = dbm.cursor(cursor_handle).raw_next()
    if query is None or query.column_type(0) != CConfig.SQLITE_INTEGER:
        return interp.space.w_nil
    object_id = query.column_int64(0)
    num_cols = query.data_count()
    obj = W_DBObject(interp.space, w_rcvr, num_cols, object_id=object_id)
    return obj
