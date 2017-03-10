from rsqueakvm.primitives import index1_0, bytelist, char, uint
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin

from rpython.rlib.rarithmetic import r_uint, intmask
from rpython.rlib import jit


class Cell(object):
    _attrs_ = ["value"]
    _immutable_fields_ = ["value?"]
    def __init__(self, value): self.value = value
    def set(self, v): self.value = v
    def get(self): return self.value


class MiscPrimitivePlugin(Plugin):
    _attrs_ = ["ascii_order"]
    _immutable_fields_ = ["ascii_order"]

    def __init__(self):
        Plugin.__init__(self)
        self.ascii_order = Cell(None)


plugin = MiscPrimitivePlugin()


@jit.look_inside_iff(lambda bytes, start: jit.isconstant(len(bytes)) and jit.isconstant(start))
def _bytesHashLoop(bytes, start):
    hash = start
    for byte in bytes:
        hash = hash + ord(byte)
        low = r_uint(hash & 16383)
        hash = (0x260D * low +
                (((0x260D * (hash >> 14) + (0x0065 * low))
                  & 16383) * 16384)) & r_uint(0x0FFFFFFF)
    return intmask(hash)


@plugin.expose_primitive(unwrap_spec=[object, bytelist, uint])
def primitiveStringHash(interp, s_frame, w_rcvr, thebytes, initialHash):
    hash = r_uint(initialHash) & r_uint(0xFFFFFFF)
    return interp.space.wrap_smallint_unsafe(_bytesHashLoop(thebytes, hash))


@jit.look_inside_iff(lambda thechar, thebytes, start: jit.isconstant(thechar) and jit.isconstant(len(thebytes)) and jit.isconstant(start))
def _indexOfLoop(thechar, thebytes, start):
    assert start >= 0
    while True:
        try:
            if thebytes[start] == thechar:
                return start + 1
        except IndexError:
            return 0
        start += 1


@plugin.expose_primitive(unwrap_spec=[object, char, bytelist, index1_0])
def primitiveIndexOfAsciiInString(interp, s_frame, w_rcvr, thechar, thebytes, start):
    if start < 0:
        raise PrimitiveFailedError
    return interp.space.wrap_smallint_unsafe(_indexOfLoop(thechar, thebytes, start))

ascii_oder = [chr(i) for i in range(256)]


def is_ascii_order(w_order):
    if w_order.getbytes() == ascii_oder:
        return True
    else:
        return False


def compare_collated(string1, string2, order):
    len1 = len(string1)
    len2 = len(string2)
    for i in range(min(len1, len2)):
        c1 = order[ord(string1[i])]
        c2 = order[ord(string2[i])]
        if c1 != c2:
            if c1 < c2:
                return 1
            else:
                return 3
    if len1 == len2:
        return 2
    if len1 < len2:
        return 1
    else:
        return 3


def compare_ascii(string1, string2):
    len1 = len(string1)
    len2 = len(string2)
    for i in range(min(len1, len2)):
        c1 = ord(string1[i])
        c2 = ord(string2[i])
        if c1 != c2:
            if c1 < c2:
                return 1
            else:
                return 3
    if len1 == len2:
        return 2
    if len1 < len2:
        return 1
    else:
        return 3


@plugin.expose_primitive(unwrap_spec=[object, bytelist, bytelist, object])
def primitiveCompareString(interp, s_frame, w_rcvr, string1, string2, w_order):
    # the first few times we do this, we spent the time to scan the order so we
    # can eventually cache the ascii order object and do ascii comparisons
    # natively.
    if not isinstance(w_order, W_BytesObject):
        raise PrimitiveFailedError
    w_cached_ascii_order = plugin.ascii_order.get()
    if w_cached_ascii_order is None:
        if is_ascii_order(w_order):
            w_cached_ascii_order = w_order
            plugin.ascii_order.set(w_order)
    if w_cached_ascii_order is w_order:
        return interp.space.wrap_smallint_unsafe(compare_ascii(string1, string2))
    return interp.space.wrap_smallint_unsafe(compare_collated(string1, string2, w_order.getbytes()))
