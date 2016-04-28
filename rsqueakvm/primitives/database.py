from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.bytecodes import SQLITE
from rsqueakvm.model.variable import W_BytesObject

import sqlite3


@expose_primitive(SQLITE)
def func(interp, s_frame, argument_count):
    if argument_count != 2:
        return interp.space.wrap_string('Two arguments expected!')

    w_arg1 = s_frame.pop()
    assert isinstance(w_arg1, W_BytesObject)
    sql_statement = interp.space.unwrap_string(w_arg1)
    w_arg2 = s_frame.pop()
    assert isinstance(w_arg2, W_BytesObject)
    dbfile = interp.space.unwrap_string(w_arg2)

    print dbfile
    print sql_statement

    conn = sqlite3.connect(dbfile)
    try:
        cursor = conn.cursor()

        cursor.execute(sql_statement)
        result = [str('; '.join(row)) for row in cursor]
    finally:
        conn.close()

    return interp.space.wrap_string('%s' % '\n '.join(result))
