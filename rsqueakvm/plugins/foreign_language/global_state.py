from rsqueakvm.error import Exit
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.cells import QuasiConstant

w_foreign_language_class = QuasiConstant(None, type=W_PointersObject)
_initialized = [False]


def startup(space):
    if _initialized[0]:
        return

    foreign_language_class = space.smalltalk_at('ForeignLanguage')
    if foreign_language_class is None:
        # disable plugin?
        error_msg = 'ForeignLanguage class not found.'
        print error_msg
        raise Exit(error_msg)
    w_foreign_language_class.set(foreign_language_class)

    _initialized[0] = True
