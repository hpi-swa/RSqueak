# -*- coding: utf-8 -*-

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi

from sqpyte.capi import CConfig


@expose_primitive(SQPYTE_EXECUTE, unwrap_spec=[object, str])
def sqpyte_execute(interp, s_frame, w_rcvr, sql_statement):
    space = interp.space

    query = interp.db_execute(sql_statement)

    # Fetch first row
    rc = query.mainloop()
    num_cols = query.data_count()

    rows = []
    # Fetch all other rows
    while rc == CConfig.SQLITE_ROW:
        row = fetch_one_row(query, space, num_cols)
        row_w = space.wrap_list(row)
        rows.append(row_w)
        rc = query.mainloop()

    return space.wrap_list(rows)


@jit.unroll_safe
def fetch_one_row(query, space, num_cols):
    cols = [None] * num_cols
    for i in range(num_cols):
        typ = query.column_type(i)
        if typ == CConfig.SQLITE_TEXT or typ == CConfig.SQLITE_BLOB:
            textlen = query.column_bytes(i)
            result = rffi.charpsize2str(
                    rffi.cast(rffi.CCHARP, query.column_text(i)), textlen)
            w_result = space.wrap_string(result)  # no encoding
        elif typ == CConfig.SQLITE_INTEGER:
            result = query.column_int64(i)
            w_result = space.wrap_int(result)
        elif typ == CConfig.SQLITE_FLOAT:
            result = query.column_double(i)
            w_result = space.wrap_float(result)
        elif typ == CConfig.SQLITE_NULL:
            w_result = space.w_nil
        else:
            raise PrimitiveFailedError
        cols[i] = w_result
    return cols


@expose_primitive(SQPYTE_CLOSE, unwrap_spec=[object, str])
def sqpyte_close(interp, s_frame, w_rcvr, sql_statement):
    return interp.space.wrap_bool(interp.db_close())


###############################################################################
# Interpreter-only, because sqlite3 cannot be compiled with RPython ¯\_(ツ)_/¯ #
###############################################################################
# from rpython.rlib import objectmodel
# if objectmodel.we_are_translated():
#     import sqlite3

#     @expose_primitive(SQLITE, unwrap_spec=[object, str, str])
#     def func(interp, s_frame, w_rcvr, db_file, sql_statement):

#         print db_file
#         print sql_statement

#         conn = sqlite3.connect(db_file)
#         try:
#             cursor = conn.cursor()

#             cursor.execute(sql_statement)
#             result = [str('; '.join(row)) for row in cursor]
#         finally:
#             conn.close()

#         return interp.space.wrap_string('%s' % '\n '.join(result))
###############################################################################
