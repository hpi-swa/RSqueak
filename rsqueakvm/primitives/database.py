# -*- coding: utf-8 -*-

from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.bytecodes import SQLITE, SQLPYTE
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX

from rpython.rtyper.lltypesystem import rffi
from sqpyte.interpreter import Sqlite3DB, Sqlite3Query
from sqpyte.capi import CConfig
from sqpyte import capi


###############################################################################
# Interpreter-only, because sqlite3 cannot be compiled with rpython ¯\_(ツ)_/¯ #
###############################################################################
# import sqlite3

# @expose_primitive(SQLITE)
# def func(interp, s_frame, argument_count):

#     w_arg1 = s_frame.pop()
#     assert isinstance(w_arg1, W_BytesObject)
#     sql_statement = interp.space.unwrap_string(w_arg1)
#     w_arg2 = s_frame.pop()
#     assert isinstance(w_arg2, W_BytesObject)
#     dbfile = interp.space.unwrap_string(w_arg2)

#     print dbfile
#     print sql_statement

#     conn = sqlite3.connect(dbfile)
#     try:
#         cursor = conn.cursor()

#         cursor.execute(sql_statement)
#         result = [str('; '.join(row)) for row in cursor]
#     finally:
#         conn.close()

#     return interp.space.wrap_string('%s' % '\n '.join(result))
###############################################################################


@expose_primitive(SQLPYTE, unwrap_spec=[object, str])
def func(interp, s_frame, w_rcvr, sql_statement):

    dbfile = interp.space.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX)
    dbfile = dbfile + '.db'

    db = Sqlite3DB(dbfile)
    query = db.execute(sql_statement)
    rc = query.mainloop()

    result = []
    while rc == CConfig.SQLITE_ROW:
        textlen1 = query.column_bytes(0)
        col1 = rffi.charpsize2str(
                rffi.cast(rffi.CCHARP, query.column_text(0)), textlen1)
        textlen2 = query.column_bytes(1)
        col2 = rffi.charpsize2str(
                rffi.cast(rffi.CCHARP, query.column_text(1)), textlen2)
        result.append('%s; %s' % (col1, col2))
        rc = query.mainloop()

    return interp.space.wrap_string('%s' % '\n '.join(result))
