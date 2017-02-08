"""
Base functions for ImmutabilityPlugin.

.. data:: WRITE_OPERATIONS
A list of all write operations to be stubbed out by `immutable_class(cls)`
decorator.
"""

from rsqueakvm.model.base import W_Object

WRITE_OPERATIONS = [
    # W_Object
    'atput0', 'store', 'store_all', 'setword' '_become', 'fillin',
    'fillin_weak', 'fillin_finalize',
    # W_PointersObject
    'pointers_become_one_way',
    # W_BytesObject / W_WordsObject
    'setchar', 'short_atput0', 'setword', 'setwords',
    'convert_to_bytes_layout', 'setbytes', 'mutate'
]


def immutable_class(cls):
    """
    This function decorates classes, so that `is_immutable` returns `True` and
    all `WRITE_OPERATIONS` are implemented as NoOps.

    :param cls: The target class.
    :returns:  The decorated class.

    """
    def is_immutable(self):
        return True
    cls.is_immutable = is_immutable

    for method_name in WRITE_OPERATIONS:
        if hasattr(cls, method_name):
            def noop(self, *args):
                pass
            setattr(cls, method_name, noop)
    return cls


def patch_w_object():
    """Add `W_Object.is_immutable` which by default returns `False`."""
    def is_immutable(self):
        return False
    W_Object.is_immutable = is_immutable
